# Sharded one-time bootstrap. Three entrypoints wired together by backfill.yml:
#   enumerate  -> build the full release roster via the NAME-LIST method (one job)
#   fetch      -> fetch getDownloadCounts for one EVEN mod-N shard (matrix)
#   merge      -> fold all shard partials into the published shards (one job)
#
# ENUMERATE uses cheap per-package-name filtered queries, not the whole-archive
# getPublishedBinaries sweep (that 503s past ~12,900 entries and is impossible).
# The name universe is current CRAN + the CRAN Archive index + Bioc VIEWS; each
# candidate r-cran-<name> / r-bioc-<name> is queried through the concurrent
# fetch_pool. FETCH shards the roster EVENLY by row index modulo N (first-letter
# buckets are wildly uneven) and fetches its slice concurrently.

if (!exists("SHARD_PREFIX")) source(file.path("scripts", "config.R"))
if (!exists("build_roster")) source(file.path("scripts", "helpers.R"))
if (!exists("run_update"))   source(file.path("scripts", "update.R"))  # default_io, with_retry

ROSTER_FILE <- "c2d4u-roster.db"
RELEASE_COLS <- c("archive","binary_name","version","pub_id","package",
                  "origin","canonical_name","identity_state","cnt_total","last_day","done")
# One-row table in every fetched part: when its fetch started (UTC) and that it
# is shard i of N. The merge counts only days settled by the earliest fetch.
PART_META_TABLE <- "c2d4u_fetch_meta"

write_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, releases_table_ddl(RELEASES_TABLE))
  if (nrow(roster_df) > 0) DBI::dbWriteTable(con, RELEASES_TABLE, roster_df[RELEASE_COLS], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

# Build the roster: every release the name-list queries find, unioned with
# every release of the currently published roster. The archive is frozen, so a
# release never legitimately disappears; the union keeps a name dropped from
# the live name indexes, or whose query failed this time, from taking its
# published history with it. Every release starts unfetched.
run_enumerate <- function(io, out_dir, live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  cran    <- io$cran_names()
  bioc    <- io$bioc_names()
  archive <- io$archive_names()
  maps    <- load_gated_maps(io, live_floor, bioc_floor)  # ledger; stops on unreachable/gate-fail
  published <- published_roster(io)
  cand <- candidate_binary_names(cran, archive, bioc)      # candidate universe unchanged
  message(sprintf("enumerate: name universe CRAN=%d CRAN-archive=%d Bioc=%d -> %d candidate names",
                  length(cran), length(archive), length(bioc), length(cand)))
  enabled <- Filter(function(a) isTRUE(a$enabled), ARCHIVES)
  ent <- do.call(rbind, lapply(enabled, function(a) enumerate_names(io$fetch_many, cand, a)))
  found <- if (is.null(ent) || nrow(ent) == 0L) .empty_releases()
           else build_roster(ent, maps)
  roster <- union_published(found, published, maps)
  message(sprintf(
    "enumerate: %d releases across %d packages (%d enumerated, %d more from the published roster of %d)",
    nrow(roster), length(unique(roster$package)), nrow(found), nrow(roster) - nrow(found),
    nrow(published)))
  write_roster(file.path(out_dir, ROSTER_FILE), roster)
}

# The roster embedded in the published release's recent shard, or an empty one
# when nothing is published yet. A release that exists but whose roster cannot
# be downloaded stops the run: enumerating without it could drop releases.
published_roster <- function(io) {
  if (!isTRUE(io$release_exists())) {
    message("enumerate: no published release, so no published roster to keep")
    return(.empty_releases())
  }
  tmp <- tempfile("c2d4u-published-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f <- sprintf("%s-recent.db", SHARD_PREFIX)
  rc <- io$release_download(f, tmp)
  if (!identical(as.integer(rc), 0L) || !file.exists(file.path(tmp, f)))
    stop("enumerate: the published release exists but its published roster (", f, ") could not ",
         "be downloaded; aborting rather than build a roster that may miss published releases. ",
         "Re-run if gh failed transiently; if a publish that stopped partway left the release ",
         "without it, complete that publish from its workflow artifact first (see the README)",
         call. = FALSE)
  load_releases(file.path(tmp, f))
}

# Add the published releases the enumeration did not find, keyed on (archive,
# binary_name, version). Their identity is refreshed from the ledger as the
# enumerated releases' is, and they start unfetched like every other release.
union_published <- function(found, published, maps) {
  key <- function(r) paste(r$archive, r$binary_name, r$version, sep = "\r")
  add <- published[!key(published) %in% key(found), RELEASE_COLS, drop = FALSE]
  if (nrow(add) == 0L) return(found)
  ident <- resolve_identities(add$binary_name, maps)
  ib <- match(add$binary_name, ident$binary_name); ok <- !is.na(ib)
  add$origin[ok]         <- ident$origin[ib[ok]]
  add$canonical_name[ok] <- ident$canonical_name[ib[ok]]
  add$identity_state[ok] <- ident$identity_state[ib[ok]]
  add$done      <- 0L
  add$last_day  <- NA_character_
  add$cnt_total <- NA_integer_
  rbind(found[RELEASE_COLS], add)
}

# Fetch getDownloadCounts for the roster's EVEN shard i-of-N (row index modulo N)
# concurrently through io$fetch_many, then retry the releases the pool could not
# fetch one at a time through io$fetch for up to retry_budget_min minutes. Writes
# the partial shard db (daily + roster slice + fetch record) that run_merge
# consumes. A release counts only when every one of its pages arrived, in the
# pool or in the retry; the rest stay done = 0 and contribute no rows. A
# release Launchpad answered with no rows is left empty once (done = 2), not
# settled as never downloaded: the next monthly update asks for it again.
run_fetch_shard <- function(io, out_dir, roster_path, i, N, retry_budget_min = RETRY_BUDGET_MIN) {
  # The merge counts only days settled by the time the fetch started.
  fetched_at <- io$now()
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_releases(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  message(sprintf("fetch shard %d/%d: %d of %d releases", i, N, nrow(mine), nrow(roster)))

  urls <- if (nrow(mine) == 0L) character(0)
          else vapply(seq_len(nrow(mine)),
            function(k) lp_counts_url(archive_by_key(mine$archive[k]), mine$pub_id[k]),
            character(1))
  res <- fetch_paginated(io$fetch_many, urls, parse_counts_page, "rows")
  rows_by <- res$data; ok <- res$ok
  failed <- which(!ok)
  # The pool keeps the pages a release got before a later page failed. Drop
  # them: the retry refetches from the first page, so keeping them would count
  # those pages twice, and a release that stays failed must add nothing.
  rows_by[failed] <- list(NULL)
  ok_pool <- sum(ok)

  started <- io$now(); recovered <- 0L
  for (k in failed) {
    if (as.numeric(difftime(io$now(), started, units = "mins")) >= retry_budget_min) {
      message(sprintf("fetch shard %d/%d: retry budget of %s min spent", i, N,
                      format(retry_budget_min)))
      break
    }
    got <- tryCatch(list(rows = paginate(io$fetch, urls[k], parse_counts_page, "rows")),
                    error = function(e) NULL)
    if (is.null(got)) next
    rows_by[k] <- list(got$rows); ok[k] <- TRUE; recovered <- recovered + 1L
  }
  message(sprintf(
    "fetch shard %d/%d: assigned %d, ok after pool %d, recovered by retry %d, still failed %d",
    i, N, nrow(mine), ok_pool, recovered, sum(!ok)))

  counts_acc <- list()
  for (k in seq_len(nrow(mine))) {
    if (!isTRUE(ok[k])) next           # failed fetch: leave done=0 to retry next run
    rows <- rows_by[[k]]
    if (is.null(rows) || nrow(rows) == 0L) {
      mine$done[k] <- 2L               # answered with no rows: empty once
      next
    }
    mine$done[k] <- 1L
    counts_acc[[length(counts_acc) + 1L]] <- rows
    mine$last_day[k] <- max(rows$day, na.rm = TRUE)
  }
  counts_all <- if (length(counts_acc)) do.call(rbind, counts_acc) else
    data.frame(binary_name = character(0), version = character(0),
               day = character(0), count = integer(0), stringsAsFactors = FALSE)
  daily <- aggregate_counts(counts_all, mine)
  # The roster's cnt_total is left unset: nothing downstream reads it, and a
  # total over one shard's releases would be wrong for any package whose
  # releases sit in several shards.
  mine$cnt_total <- rep(NA_integer_, nrow(mine))
  sp <- write_part(file.path(out_dir, sprintf("%s-shard-%d.db", SHARD_PREFIX, i)),
                   daily, mine, fetched_at, i, N)
  message(sprintf(paste0("fetch shard %d/%d: %d daily rows, %d releases fetched, %d of them ",
                         "with no downloads"),
                  i, N, nrow(daily), sum(mine$done %in% 1:2), count_empty_once(mine$done)))
  sp
}

# Write a fetched part: its daily rows, its roster slice (so the merge can
# reassemble the roster) and its fetch record.
write_part <- function(path, daily, releases, fetched_at, i, N) {
  export_shard(path, daily)
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, releases_table_ddl(RELEASES_TABLE))
  if (nrow(releases) > 0) DBI::dbWriteTable(con, RELEASES_TABLE, releases[RELEASE_COLS], append = TRUE)
  write_part_meta(path, fetched_at, i, N)
  path
}

# The one-row fetch record of a part: when its fetch started (UTC) and that it
# is shard i of N. Replaces any record the part already holds.
write_part_meta <- function(path, fetched_at, i, N) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", PART_META_TABLE))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (fetched_at TEXT NOT NULL, shard_i INTEGER NOT NULL, shard_n INTEGER NOT NULL)",
    PART_META_TABLE))
  DBI::dbWriteTable(con, PART_META_TABLE,
    data.frame(fetched_at = iso(fetched_at), shard_i = as.integer(i), shard_n = as.integer(N),
               stringsAsFactors = FALSE), append = TRUE)
  invisible(path)
}

# A part's fetch record, or a stop naming the part when it has none: without
# one the merge cannot tell which days were settled when the part was fetched.
read_part_meta <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  meta <- if (PART_META_TABLE %in% DBI::dbListTables(con))
    DBI::dbGetQuery(con, sprintf("SELECT fetched_at, shard_i, shard_n FROM %s", PART_META_TABLE))
  if (is.null(meta) || nrow(meta) != 1L)
    stop(basename(path), " has no fetch record (", PART_META_TABLE, "); it was not written ",
         "by this backfill's fetch step, so its settled days are unknown", call. = FALSE)
  meta
}

# Fold the fetched parts into the published release. Every release's downloads
# are SUMMED per (package, date): a package's releases are spread over the
# shards by roster row, so each part holds only a partial count of its days.
# Only days settled when the earliest part was fetched are counted, and the
# summary is anchored on that last counted day, not on the merge's own clock
# (a re-run days later must not claim coverage the fetch never had). The end
# of the history is then judged as the monthly update judges its window
# (merge_tail): days after a Launchpad stall began are not counted yet, and a
# quiet stretch long enough to count as downloads that stopped is recorded.
#
# Nothing is written unless the result clears the publish floor: exactly the
# parts of shards 0..N-1, whose rosters add up to the enumerated roster at
# roster_path; at most UNFETCHED_MAX_FRAC of releases unfetched; and, when a
# release is published, a history that dominates it (check_dominance).
run_merge <- function(io, out_dir, parts_dir, N, roster_path) {
  parts <- expected_parts(parts_dir, N)
  metas <- lapply(parts, read_part_meta)
  for (k in seq_along(parts)) {
    m <- metas[[k]]
    if (!identical(as.integer(m$shard_i), k - 1L) || !identical(as.integer(m$shard_n), as.integer(N)))
      stop(sprintf("merge: %s says it is shard %s of %s", basename(parts[k]), m$shard_i, m$shard_n),
           call. = FALSE)
  }
  fetched <- as.Date(substr(vapply(metas, function(m) m$fetched_at, character(1)), 1, 10))
  if (anyNA(fetched)) stop("merge: a part's fetch record has no readable fetched_at", call. = FALSE)
  through <- format(min(fetched) - SETTLED_LAG_DAYS, "%Y-%m-%d")

  roster <- do.call(rbind, c(list(.empty_releases()), lapply(parts, load_releases)))
  if (!file.exists(roster_path))
    stop("merge: the enumerated roster ", roster_path, " is missing", call. = FALSE)
  enumerated <- nrow(load_releases(roster_path))
  if (nrow(roster) != enumerated)
    stop(sprintf("merge: the parts hold %d roster rows, but the enumerated roster has %d",
                 nrow(roster), enumerated), call. = FALSE)
  if (nrow(roster) == 0L) stop("merge: the roster is empty; there is nothing to publish", call. = FALSE)
  # A release answered empty once was fetched: it counts toward
  # empty_once_releases, not toward the unfetched floor.
  unfetched <- count_unfetched(roster$done)
  empty_once <- count_empty_once(roster$done)
  check_unfetched(unfetched, nrow(roster))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sum_parts(con, parts, through)
  summary_df <- build_summary(con, roster, through, prior_summary = NULL)
  years <- part_years(con)
  published <- if (isTRUE(io$release_exists()))
    check_dominance(io, con, roster, summary_df, years, through)
  else message("merge: no published release to compare against; publishing the first one")
  tail <- merge_tail(con, through, published)
  if (!identical(tail$through, through)) {
    # The days after it hold no downloads; a row there could only be a zero.
    through <- tail$through
    if (DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE date > ?", DAILY_TABLE),
                       params = list(through)) > 0L)
      years <- part_years(con)
    summary_df <- build_summary(con, roster, through, prior_summary = NULL)
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  recent_path  <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))
  changed <- character(0)
  for (yr in years) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    export_shard(file.path(out_dir, f), extract_year(con, as.integer(yr)))
    changed <- c(changed, f)
  }
  export_shard(recent_path, extract_recent(con, through, RECENT_WINDOW_DAYS))
  embed_aux(recent_path, summary_df, roster)
  export_summary_shard(summary_path, summary_df)
  changed <- c(changed, basename(recent_path), basename(summary_path))

  now <- io$now()
  keys <- vapply(Filter(function(a) isTRUE(a$enabled), ARCHIVES), function(a) a$key, character(1))
  base <- list(
    tag = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(now), last_checked = iso(now), last_changed = iso(now),
    source_kind = "launchpad", archives = as.list(keys),
    changed_shards = as.list(changed), shards = list(),
    summary = list(packages = nrow(summary_df), latest_date = through, releases = nrow(roster)))
  # A fresh backfill: nothing held back, and no refetch floor of its own: the
  # next update's window covers the revision window before counted_through,
  # and every older day was fetched here long after it ended. The gaps and the
  # floor are merge_tail's: the published gaps and a stretch counted as stopped.
  # A package the published release held keeps its floor (held_floors).
  out <- contract_manifest(base, out_dir, changed, through, package_edges = list(),
                           unfetched_releases = unfetched, empty_once_releases = empty_once,
                           detected_gaps = tail$gaps, refetch_from = tail$floor,
                           package_refetch_from = held_floors(published, tail$floor))
  write_manifest(file.path(out_dir, "manifest.json"), out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  write_upload_list(out_dir, changed)
  tot <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n, SUM(count) AS s FROM %s", DAILY_TABLE))
  message(sprintf(paste0("merge: %d parts, %s package-days, %s downloads, counted through %s, ",
                         "%d of %d releases unfetched, %d answered empty once"),
                  length(parts), format(tot$n, big.mark = ","), format(tot$s, big.mark = ",", scientific = FALSE),
                  through, unfetched, nrow(roster), empty_once))
  list(changed_shards = changed, manifest = out)
}

# The part files of shards 0..N-1 in parts_dir, stopping on a missing part
# (its share of the roster would vanish from the history) or on one not
# expected (a part of another run or another N).
expected_parts <- function(parts_dir, N) {
  N <- suppressWarnings(as.integer(N))
  if (length(N) != 1L || is.na(N) || N < 1L) stop("merge: N must be a positive number of shards")
  want <- sprintf("%s-shard-%d.db", SHARD_PREFIX, seq_len(N) - 1L)
  have <- list.files(parts_dir, pattern = sprintf("^%s-shard-.*\\.db$", SHARD_PREFIX))
  problems <- c(sprintf("missing %s", setdiff(want, have)), sprintf("unexpected %s", setdiff(have, want)))
  if (length(problems))
    stop(sprintf("merge: expected the parts of shards 0..%d in %s: %s", N - 1L, parts_dir,
                 paste(problems, collapse = "; ")), call. = FALSE)
  file.path(parts_dir, want)
}

# Refuse a history with more than UNFETCHED_MAX_FRAC of its releases unfetched,
# and publish one above UNFETCHED_WARN_FRAC with a GitHub warning annotation.
check_unfetched <- function(unfetched, releases) {
  frac <- unfetched / releases
  what <- sprintf("%d of %d releases (%.1f%%) are unfetched", unfetched, releases, 100 * frac)
  if (frac > UNFETCHED_MAX_FRAC)
    stop(sprintf("merge: %s, more than the %.0f%% a backfill may publish; re-run backfill.yml",
                 what, 100 * UNFETCHED_MAX_FRAC), call. = FALSE)
  if (frac > UNFETCHED_WARN_FRAC) {
    cat(sprintf("::warning::c2d4u backfill: %s; they count toward unfetched_releases\n", what))
    message("merge: ", what)
  }
  invisible(frac)
}

# The new history must dominate the release it replaces. Sums over every
# release can never count fewer downloads than the published history counted
# (the first-wins history kept one shard's partial of each package-day, a
# lower bound of the sum), so a shortfall means releases or days went missing.
# Stops when the parts count through (`through`) an earlier day than the
# published edge: they were fetched before that release was published (the
# merge job re-run after a monthly update), and publishing them would move
# counted_through back and lose the days in between, which the dominance
# tolerance below would let through for a few busy packages. Stops also on
# fewer releases or packages than published, on a published year shard this
# history has no rows for (the stale asset would stay on the release), and
# when more than DOMINANCE_MAX_FRAC of the published packages count fewer
# downloads through the published edge than the published cnt_total; fewer
# than that are listed in the log.
#
# A publish uploads the database assets one at a time and manifest.json last,
# and `gh release upload --clobber` deletes an asset before uploading its
# replacement, so a publish that stopped partway can leave newer assets next
# to an older manifest, or an asset missing. The check reads what is there:
# the per-package totals from the summary DB, else from the copy of the
# summary the recent shard carries; the release and package floors from the
# manifest, else from the published roster and summary. Only when neither
# summary can be read does it refuse.
#
# Returns, invisibly, what the merge carries over from the published
# manifest: its counted_through (else, on a release from before the summed
# history, summary.latest_date; NULL without a manifest) and detected_gaps
# for merge_tail, its package_edges and refetch floors for held_floors, with
# `bad`, the packages that count fewer downloads.
check_dominance <- function(io, con, roster, summary_df, years, through) {
  tmp <- tempfile("c2d4u-published-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  sf <- sprintf("%s-summary.db", SHARD_PREFIX)
  rf <- sprintf("%s-recent.db", SHARD_PREFIX)
  got <- function(f) identical(as.integer(io$release_download(f, tmp)), 0L) &&
                     file.exists(file.path(tmp, f))
  have <- vapply(c("manifest.json", sf, rf), got, logical(1))
  if (!have[[sf]] && !have[[rf]])
    stop("merge: a release is published but its summary (", sf, ") and its recent shard could ",
         "not be downloaded; refusing to replace a release this merge cannot compare against",
         call. = FALSE)
  prev <- if (have[["manifest.json"]])
    jsonlite::fromJSON(file.path(tmp, "manifest.json"), simplifyVector = FALSE)
  else {
    message("merge: the published release has no manifest.json (a publish stopped before its ",
            "last upload); comparing against its summary and roster only")
    list()
  }
  if (!have[[sf]])
    message("merge: the published release has no ", sf, "; comparing against the summary its ",
            "recent shard carries")
  prev_summary <- load_summary(file.path(tmp, if (have[[sf]]) sf else rf))
  carried <- list(counted_through = prev$counted_through %||% prev$summary$latest_date,
                  detected_gaps = prev$detected_gaps %||% list(),
                  package_edges = prev$package_edges %||% list(),
                  refetch_from = prev$refetch_from,
                  package_refetch_from = prev$package_refetch_from %||% list())

  # The published history ends at its edge; later days are new, not a check.
  # cnt_total counts the summary's own rows, which end on its latest
  # last_date, so the edge is never earlier than that: a publish torn after its
  # summary landed pairs that newer summary with the older manifest's
  # counted_through, and comparing the newer totals through the older edge
  # would refuse every backfill that could repair the release.
  known <- c(as.character(prev$counted_through %||% prev$summary$latest_date),
             prev_summary$last_date)
  known <- known[!is.na(known)]
  edge <- if (length(known)) max(known) else NA_character_
  if (!is.na(edge) && through < edge)
    stop(sprintf(paste0("merge: the parts predate the published release: they count through %s, ",
                        "before its %s, so publishing them would move counted_through back and ",
                        "lose the days in between. Re-run the whole backfill.yml (enumerate, fetch ",
                        "and merge), not just its merge job"), through, edge), call. = FALSE)

  prev_releases <- prev$summary$releases %||%
    (if (have[[rf]]) nrow(load_releases(file.path(tmp, rf))))
  if (!is.null(prev_releases) && nrow(roster) < prev_releases)
    stop(sprintf("merge: the roster has %d releases, fewer than the %d of the published release",
                 nrow(roster), as.integer(prev_releases)), call. = FALSE)
  prev_packages <- prev$summary$packages %||% nrow(prev_summary)
  if (nrow(summary_df) < prev_packages)
    stop(sprintf("merge: the history has %d packages, fewer than the %d of the published release",
                 nrow(summary_df), as.integer(prev_packages)), call. = FALSE)
  year_rx <- sprintf("^%s-(20[0-9]{2})\\.db$", SHARD_PREFIX)
  prev_years <- grep(year_rx, names(prev$shards %||% list()), value = TRUE)
  left <- setdiff(prev_years, sprintf("%s-%s.db", SHARD_PREFIX, years))
  if (length(left))
    stop("merge: the published release has ", paste(left, collapse = ", "),
         " but this history has no rows for that year", call. = FALSE)
  if (nrow(prev_summary) == 0L) return(invisible(c(carried, list(bad = integer(0)))))

  sums <- DBI::dbGetQuery(con, sprintf(
    "SELECT package, SUM(count) AS n FROM %s WHERE date <= ? GROUP BY package", DAILY_TABLE),
    params = list(as.character(edge)))
  new_n <- as.numeric(sums$n[match(prev_summary$package, sums$package)])
  new_n[is.na(new_n)] <- 0
  old_n <- as.numeric(prev_summary$cnt_total); old_n[is.na(old_n)] <- 0
  bad <- which(new_n < old_n)
  what <- sprintf("%d of %d packages (%.1f%%) count fewer downloads through %s than the published release",
                  length(bad), nrow(prev_summary), 100 * length(bad) / max(1L, nrow(prev_summary)), edge)
  listed <- sprintf("%s (%s < %s)", prev_summary$package[bad],
                    format(new_n[bad], scientific = FALSE), format(old_n[bad], scientific = FALSE))
  if (length(bad) > DOMINANCE_MAX_FRAC * nrow(prev_summary))
    stop(sprintf("merge: %s, more than %.0f%%: %s%s", what, 100 * DOMINANCE_MAX_FRAC,
                 paste(utils::head(listed, 20L), collapse = ", "),
                 if (length(listed) > 20L) ", ..." else ""), call. = FALSE)
  message(sprintf("merge: %s%s", what, if (length(bad)) paste0(": ", paste(listed, collapse = ", ")) else ""))
  invisible(c(carried, list(bad = bad)))
}

# Judge the end of the summed history as the monthly update judges its
# window. `through` is the earliest fetch day less SETTLED_LAG_DAYS, whatever
# Launchpad had reported by then, so a backfill fetched during a stall would
# otherwise store the days Launchpad had not reported yet as zeros and count
# past them, and no monthly window, which starts REVISION_WINDOW_DAYS before
# counted_through, would fetch them again. So, with the update's window_gaps:
#   - a trailing run of at least ZERO_RUN_DAYS days with no downloads anywhere
#     outside a known gap is a stall: the history counts through the day
#     before it, and the monthly update fetches those days as new ones.
#   - past STALL_MAX_DAYS it is downloads that stopped: counted through, and
#     recorded in detected_gaps with kind "stopped" from its first quiet day,
#     which is the refetch floor.
# `published` (check_dominance's result, NULL without a published release)
# carries the published detected_gaps over. When they end on a stretch counted
# as stopped at the published counted_through, it is judged again from its
# first day, as the update does: days Launchpad filled in trim it (their runs
# still empty stay recorded), and the stretch's old first day stays the floor,
# so the next update fetches the filled days a second time. counted_through
# never moves back: when the quiet began before the published counted_through,
# the history counts through that day and the published gaps stay as they
# were. Returns list(through, gaps, floor) with through and floor as
# YYYY-MM-DD (floor NULL for none).
merge_tail <- function(con, through, published = NULL) {
  through <- as.Date(through)
  gaps <- published$detected_gaps %||% list()
  ct_pub <- as.Date(as.character(published$counted_through %||% NA), format = "%Y-%m-%d")
  last_gap <- if (length(gaps)) gaps[[length(gaps)]]
  pending <- !is.na(ct_pub) && identical(last_gap$kind, "stopped") &&
             identical(as.character(last_gap$to), format(ct_pub))
  stopped_from <- if (pending) as.Date(as.character(last_gap$from), format = "%Y-%m-%d")
                  else as.Date(NA)
  last <- DBI::dbGetQuery(con, sprintf("SELECT MAX(date) AS d FROM %s WHERE count > 0",
                                       DAILY_TABLE))$d
  last <- as.Date(if (is.null(last)) NA_character_ else as.character(last))
  if (is.na(last) && !pending)
    return(list(through = format(through), gaps = gaps, floor = NULL))
  gfrom <- if (pending && !is.na(stopped_from)) stopped_from - 1 else last - 1
  tot <- DBI::dbGetQuery(con, sprintf(
    "SELECT date, SUM(count) AS n FROM %s WHERE date > ? GROUP BY date", DAILY_TABLE),
    params = list(format(gfrom)))
  g <- window_gaps(stats::setNames(as.numeric(tot$n), tot$date), gfrom, through,
                   last_seen = if (!is.na(last) && last <= gfrom) format(last) else NA)
  if (!is.na(ct_pub) && g$t_eff < ct_pub) {
    if (!is.null(g$trailing))
      cat(sprintf(paste0("::warning::c2d4u backfill: Launchpad reported no downloads from %s to ",
                         "%s; counted_through stays at %s\n"),
                  g$trailing$from, g$trailing$to, format(ct_pub)))
    return(list(through = format(ct_pub), gaps = gaps,
                floor = if (pending) format(stopped_from)))
  }
  if (!is.null(g$trailing))
    cat(sprintf(paste0("::warning::c2d4u backfill: Launchpad reported no downloads from %s to ",
                       "%s; counted_through stops at %s\n"),
                g$trailing$from, g$trailing$to, format(g$t_eff)))
  if (!is.null(g$stopped))
    cat(sprintf(paste0("::warning::c2d4u backfill: Launchpad reported no downloads from %s to ",
                       "%s, more than %d days outside the known gaps; counting that as ",
                       "downloads having stopped\n"),
                g$stopped$from, g$stopped$to, STALL_MAX_DAYS))
  gaps <- add_gaps(if (pending) gaps[-length(gaps)] else gaps, g$detected)
  last_gap <- if (length(gaps)) gaps[[length(gaps)]]
  floor <- c(if (identical(last_gap$kind, "stopped") && identical(last_gap$to, format(g$t_eff)))
               last_gap$from,
             if (pending && any(tot$n > 0 & tot$date <= format(ct_pub))) format(stopped_from))
  list(through = format(g$t_eff), gaps = gaps, floor = if (length(floor)) min(floor))
}

# The refetch floors a backfill keeps from the published release (`published`,
# check_dominance's result), for packages that release held. The backfill
# fetches every package's days again: for a package that stored the days a
# published floor covers when the floor was set, that is their second look. A
# held package stored nothing then (days Launchpad filled in inside a stopped
# stretch while it was held are the case), so this fetch may be the first to
# store them. As in the monthly update (next_floors), it keeps the floor it
# would have been refreshed from, when that reaches back past its revision
# window, for one more run. Only floors earlier than `all`, the backfill's own
# floor for every package, are listed.
held_floors <- function(published, all = NULL) {
  edges <- published$package_edges %||% list()
  if (length(edges) == 0L) return(list())
  held <- names(edges)
  floor_all <- as.Date(as.character(published$refetch_from %||% NA), format = "%Y-%m-%d")
  f <- package_floor(held, floor_all, published$package_refetch_from %||% list())
  keep <- !is.na(f) & f < as.Date(unlist(edges, use.names = FALSE)) - REVISION_WINDOW_DAYS
  if (!is.null(all)) keep <- keep & f < as.Date(all)
  as.list(stats::setNames(format(f[keep]), held[keep]))
}

# Load every part's daily rows dated on or before `through` into a staging
# table without a key, then sum them per (package, date) into the daily table.
# Done in SQLite: the full history is about 9.5M part rows. Parts are attached
# one at a time (SQLite allows only a handful of attached databases at once).
sum_parts <- function(con, parts, through) {
  DBI::dbExecute(con, "CREATE TABLE staging (package TEXT NOT NULL, date TEXT NOT NULL,
                                            count INTEGER NOT NULL)")
  for (p in parts) {
    DBI::dbExecute(con, "ATTACH DATABASE ? AS part", params = list(normalizePath(p)))
    DBI::dbExecute(con, sprintf(
      "INSERT INTO staging SELECT package, date, count FROM part.%s WHERE date <= ?", DAILY_TABLE),
      params = list(through))
    DBI::dbExecute(con, "DETACH DATABASE part")
  }
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  DBI::dbExecute(con, sprintf(
    "INSERT INTO %s SELECT package, date, SUM(count) FROM staging GROUP BY package, date", DAILY_TABLE))
  DBI::dbExecute(con, "DROP TABLE staging")
  invisible(con)
}

part_years <- function(con)
  DBI::dbGetQuery(con, sprintf("SELECT DISTINCT substr(date, 1, 4) AS y FROM %s ORDER BY y",
                               DAILY_TABLE))$y

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) >= 1L) args[[1]] else ""
  out_dir <- Sys.getenv("C2D4U_OUT", "out")
  io <- default_io()
  if (mode == "enumerate") {
    run_enumerate(io, out_dir)
  } else if (mode == "fetch") {
    i <- suppressWarnings(as.integer(Sys.getenv("C2D4U_SHARD_I", "0")))
    N <- suppressWarnings(as.integer(Sys.getenv("C2D4U_SHARD_N", "1")))
    if (is.na(i) || is.na(N) || N < 1L || i < 0L || i >= N)
      stop("fetch: C2D4U_SHARD_I must be in [0, C2D4U_SHARD_N)")
    budget <- suppressWarnings(as.numeric(Sys.getenv("C2D4U_RETRY_BUDGET_MIN",
                                                     as.character(RETRY_BUDGET_MIN))))
    if (is.na(budget) || budget < 0) stop("fetch: C2D4U_RETRY_BUDGET_MIN must be a number of minutes")
    run_fetch_shard(io, out_dir, file.path(Sys.getenv("C2D4U_ROSTER", out_dir), ROSTER_FILE), i, N,
                    retry_budget_min = budget)
  } else if (mode == "merge") {
    # N and the enumerated roster are what the publish floor checks the parts
    # against, so neither has a default.
    N <- suppressWarnings(as.integer(Sys.getenv("C2D4U_SHARD_N", "")))
    if (is.na(N) || N < 1L) stop("merge: set C2D4U_SHARD_N to the number of fetch shards")
    if (!nzchar(Sys.getenv("C2D4U_ROSTER")))
      stop("merge: set C2D4U_ROSTER to the directory holding the enumerated ", ROSTER_FILE)
    run_merge(io, out_dir, Sys.getenv("C2D4U_PARTS", "parts"), N = N,
              roster_path = file.path(Sys.getenv("C2D4U_ROSTER"), ROSTER_FILE))
  } else stop("usage: backfill.R [enumerate|fetch|merge]")
}
