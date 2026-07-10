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

write_roster <- function(path, roster_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, releases_table_ddl(RELEASES_TABLE))
  cols <- c("archive","binary_name","version","pub_id","package",
            "origin","canonical_name","identity_state","cnt_total","last_day","done")
  if (nrow(roster_df) > 0) DBI::dbWriteTable(con, RELEASES_TABLE, roster_df[cols], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

run_enumerate <- function(io, out_dir, live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  cran    <- io$cran_names()
  bioc    <- io$bioc_names()
  archive <- io$archive_names()
  maps    <- load_gated_maps(io, live_floor, bioc_floor)  # ledger; stops on unreachable/gate-fail
  cand <- candidate_binary_names(cran, archive, bioc)      # candidate universe unchanged
  message(sprintf("enumerate: name universe CRAN=%d CRAN-archive=%d Bioc=%d -> %d candidate names",
                  length(cran), length(archive), length(bioc), length(cand)))
  enabled <- Filter(function(a) isTRUE(a$enabled), ARCHIVES)
  ent <- do.call(rbind, lapply(enabled, function(a) enumerate_names(io$fetch_many, cand, a)))
  roster <- if (is.null(ent) || nrow(ent) == 0L) .empty_releases()
            else build_roster(ent, maps)
  message(sprintf("enumerate: %d releases across %d packages",
                  nrow(roster), length(unique(roster$package))))
  write_roster(file.path(out_dir, ROSTER_FILE), roster)
}

# Fetch getDownloadCounts for the roster's EVEN shard i-of-N (row index modulo N)
# concurrently through io$fetch_many. Writes the same partial shard db (daily +
# roster slice) that run_merge consumes.
run_fetch_shard <- function(io, out_dir, roster_path, i, N) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roster <- load_releases(roster_path)
  mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
  message(sprintf("fetch shard %d/%d: %d of %d releases", i, N, nrow(mine), nrow(roster)))

  urls <- if (nrow(mine) == 0L) character(0)
          else vapply(seq_len(nrow(mine)),
            function(k) lp_counts_url(archive_by_key(mine$archive[k]), mine$pub_id[k]),
            character(1))
  res <- fetch_paginated(io$fetch_many, urls, parse_counts_page, "rows")

  counts_acc <- list()
  for (k in seq_len(nrow(mine))) {
    if (!isTRUE(res$ok[k])) next       # failed fetch: leave done=0 to retry next run
    mine$done[k] <- 1L                 # fetched successfully
    rows <- res$data[[k]]
    if (is.null(rows) || nrow(rows) == 0L) next   # fetched, but no downloads recorded
    counts_acc[[length(counts_acc) + 1L]] <- rows
    mine$last_day[k] <- max(rows$day, na.rm = TRUE)
  }
  counts_all <- if (length(counts_acc)) do.call(rbind, counts_acc) else
    data.frame(binary_name = character(0), version = character(0),
               day = character(0), count = integer(0), stringsAsFactors = FALSE)
  daily <- aggregate_counts(counts_all, mine)
  # per-package cnt_total for this shard from the fetched rows
  if (nrow(daily) > 0) {
    tot <- stats::aggregate(count ~ package, data = daily, FUN = sum)
    mine$cnt_total <- as.integer(tot$count[match(mine$package, tot$package)])
  }
  sp <- file.path(out_dir, sprintf("%s-shard-%d.db", SHARD_PREFIX, i))
  export_shard(sp, daily)
  # attach the roster slice so merge can reassemble state
  con <- DBI::dbConnect(RSQLite::SQLite(), sp); on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, releases_table_ddl(RELEASES_TABLE))
  cols <- c("archive","binary_name","version","pub_id","package",
            "origin","canonical_name","identity_state","cnt_total","last_day","done")
  if (nrow(mine) > 0) DBI::dbWriteTable(con, RELEASES_TABLE, mine[cols], append = TRUE)
  message(sprintf("fetch shard %d/%d: %d daily rows, %d releases fetched",
                  i, N, nrow(daily), sum(mine$done)))
  sp
}

run_merge <- function(io, out_dir, parts_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  parts <- list.files(parts_dir, full.names = TRUE,
    pattern = sprintf("^%s-shard-.*\\.db$", SHARD_PREFIX))
  daily_all <- do.call(rbind, lapply(parts, load_daily))
  roster    <- do.call(rbind, lapply(parts, load_releases))
  if (is.null(daily_all)) daily_all <- load_daily(tempfile())
  if (is.null(roster)) roster <- .empty_releases()
  daily_all <- daily_all[!duplicated(paste(daily_all$package, daily_all$date)), , drop = FALSE]

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(daily_all) > 0) DBI::dbWriteTable(con, DAILY_TABLE, daily_all[c("package","date","count")], append = TRUE)

  anchor <- if (nrow(daily_all) > 0) max(daily_all$date) else format(as.Date(io$now()))
  now <- io$now()
  summary_df <- build_summary(con, roster, anchor, prior_summary = NULL)

  recent_path  <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))
  changed <- character(0); shard_updates <- list()
  for (yr in sort(unique(substr(daily_all$date, 1, 4)))) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    rows <- extract_year(con, as.integer(yr))
    export_shard(file.path(out_dir, f), rows)
    changed <- c(changed, f); shard_updates[[f]] <- coverage(rows)
  }
  r_rows <- extract_recent(con, anchor, RECENT_WINDOW_DAYS)
  export_shard(recent_path, r_rows)
  embed_aux(recent_path, summary_df, roster)
  export_summary_shard(summary_path, summary_df)
  changed <- c(changed, basename(recent_path), basename(summary_path))
  shard_updates[[basename(recent_path)]] <- coverage(r_rows)

  out <- list(
    tag = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(now), last_checked = iso(now), last_changed = iso(now),
    source_kind = "launchpad",
    changed_shards = as.list(changed), shards = shard_updates,
    summary = list(packages = nrow(summary_df), latest_date = anchor, releases = nrow(roster)))
  write_manifest(file.path(out_dir, "manifest.json"), out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  list(changed_shards = changed, manifest = out)
}

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
    run_fetch_shard(io, out_dir, file.path(Sys.getenv("C2D4U_ROSTER", out_dir), ROSTER_FILE), i, N)
  } else if (mode == "merge") {
    run_merge(io, out_dir, Sys.getenv("C2D4U_PARTS", "parts"))
  } else stop("usage: backfill.R [enumerate|fetch|merge]")
}
