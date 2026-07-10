# Monthly change-gated refresh for the c2d4u-downloads pipeline. The heavy one-time
# bootstrap lives in backfill.R (sharded). run_update loads the full published
# history (every year shard) plus the roster, re-fetches only the active tail of
# releases (start_date-bounded), and rebuilds the changed shards. All network and
# gh access is behind the injectable `io` so run_update runs fully offline in tests.

if (!exists("SHARD_PREFIX")) source(file.path("scripts", "config.R"))
if (!exists("build_roster")) source(file.path("scripts", "helpers.R"))

with_retry <- function(expr, tries = 9L, wait = 5) {
  # Exponential backoff (capped) so a sustained Launchpad 503 during a long
  # multi-page sweep is ridden out rather than aborting the whole enumeration.
  # Total window ~ 5+10+20+40+80+120+120+120 = ~8.5 min of retries per request.
  for (i in seq_len(tries)) {
    val <- tryCatch(force(expr), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < tries) Sys.sleep(min(wait * 2^(i - 1), 120))
  }
  stop(val)
}

run_update <- function(io, out_dir, force_full = FALSE, reclassify_only = FALSE,
                       live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  recent_path   <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path  <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))
  manifest_path <- file.path(out_dir, "manifest.json")

  # PROTECT-HISTORY: pull manifest + recent shard + every year shard.
  if (io$release_exists()) {
    mc <- io$release_download("manifest.json", out_dir)
    rc <- io$release_download(basename(recent_path), out_dir)
    if (!identical(as.integer(mc), 0L) || !file.exists(manifest_path) ||
        !identical(as.integer(rc), 0L) || !file.exists(recent_path)) {
      stop("release 'current' exists but its manifest/recent shard could not be ",
           "downloaded; aborting to protect accumulated history")
    }
    io$release_download(sprintf("%s-20*.db", SHARD_PREFIX), out_dir)  # year shards (best effort)
  }
  prev <- if (file.exists(manifest_path))
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE) else list()
  prev_shards <- prev$shards %||% list()

  now <- io$now(); today <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
  roster        <- load_releases(recent_path)
  prior_summary <- load_summary(recent_path)

  # reclassify-only rebuilds identity from the ledger onto an already-published
  # roster; it never crawls Launchpad, so an empty roster means there is nothing
  # to reclassify (never silently no-op into a heartbeat).
  if (isTRUE(reclassify_only) && nrow(roster) == 0L)
    stop("reclassify-only needs an existing roster; no recent shard was loaded")

  # Load the org identity ledger once for this run (size-gated). On any failure
  # DEGRADE honestly: keep the token as canonical_name and NA identity_state, never
  # abort and never drop a row. The frozen archive self-heals on the next run.
  # EXCEPTION: reclassify-only must never degrade -- the entire point of the run
  # is to (re)apply the ledger, so a missing/unreachable ledger there is fatal.
  ledger <- tryCatch(load_gated_maps(io, live_floor, bioc_floor),
                     error = function(e) {
                       if (isTRUE(reclassify_only))
                         stop("reclassify-only requires the identity ledger; aborting rather ",
                              "than republish degraded identity (", conditionMessage(e), ")")
                       message("identity ledger unavailable (", conditionMessage(e),
                               "); degrading to token canonical_name and NA identity_state")
                       NULL
                     })
  resolver <- ledger %||% empty_identity_maps()

  year_files <- list.files(out_dir, full.names = TRUE,
    pattern = sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX))
  # Always union the recent shard as a history floor: a partial year-shard
  # download must never let a touched year's re-export drop earlier history.
  daily_hist <- do.call(rbind, c(lapply(year_files, load_daily), list(load_daily(recent_path))))
  daily_hist <- daily_hist[!duplicated(paste(daily_hist$package, daily_hist$date)), , drop = FALSE]

  heartbeat <- function(reason) {
    out <- if (length(prev) > 0) prev else list()
    out$last_checked <- iso(now); out$source_kind <- "frozen"; out$changed_shards <- list()
    write_manifest(manifest_path, out)
    write_release_notes(file.path(out_dir, "release_notes.md"), out)
    message("heartbeat: ", reason)
    list(changed_shards = character(0), manifest = out)
  }

  # ENUMERATE only when cold or forced (the archive is frozen; the roster is static).
  # Never in reclassify-only mode: zero Launchpad calls.
  if (!isTRUE(reclassify_only) && (nrow(roster) == 0L || isTRUE(force_full))) {
    enabled <- Filter(function(a) isTRUE(a$enabled), ARCHIVES)
    # Enumerate each archive independently so one unreachable archive does not
    # abort the roster for the others.
    acc <- lapply(enabled, function(a)
      tryCatch(enumerate_archive(io$fetch, a), error = function(e) NULL))
    acc <- Filter(Negate(is.null), acc)
    ent <- if (length(acc)) do.call(rbind, acc) else NULL
    if (!is.null(ent) && nrow(ent) > 0L) {
      add <- build_roster(ent, resolver)
      ko <- paste(roster$archive, roster$binary_name, roster$version)
      kn <- paste(add$archive, add$binary_name, add$version)
      roster <- rbind(roster, add[!kn %in% ko, , drop = FALSE])
    }
  }
  if (nrow(roster) == 0L) {
    if (!io$release_exists()) stop("cold start but no releases enumerated")
    return(heartbeat("empty roster"))
  }

  # ACTIVE tail: releases never fetched, or with a recent last_day, or force_full.
  # Skipped entirely in reclassify-only mode: no io$fetch/paginate at all, so
  # zero Launchpad calls. active is forced empty (active_releases = 0 in the
  # manifest) and daily_new stays empty; the summary rebuilds from daily_hist.
  if (isTRUE(reclassify_only)) {
    active <- roster[0, , drop = FALSE]
    daily_new <- data.frame(package = character(0), date = character(0),
                            count = integer(0), stringsAsFactors = FALSE)
  } else {
    active_cut <- format(today - ACTIVE_WINDOW_DAYS, "%Y-%m-%d")
    is_active <- isTRUE(force_full) | roster$done == 0L | is.na(roster$last_day) |
                 (!is.na(roster$last_day) & roster$last_day >= active_cut)
    active <- roster[is_active, , drop = FALSE]

    counts_acc <- list(); any_fetch <- FALSE
    for (i in seq_len(nrow(active))) {
      r <- active[i, ]; a <- archive_by_key(r$archive)
      sd <- if (isTRUE(force_full) || is.na(r$last_day)) NULL
            else format(as.Date(r$last_day) - REVISION_WINDOW_DAYS, "%Y-%m-%d")
      rows <- tryCatch(
        paginate(io$fetch, lp_counts_url(a, r$pub_id, start_date = sd), parse_counts_page, "rows"),
        error = function(e) NULL)
      if (is.null(rows)) next
      any_fetch <- TRUE
      idx <- which(roster$archive == r$archive & roster$binary_name == r$binary_name &
                   roster$version == r$version)
      roster$done[idx] <- 1L
      if (nrow(rows) > 0L) {
        counts_acc[[length(counts_acc) + 1L]] <- rows
        roster$last_day[idx] <- max(c(r$last_day, rows$day), na.rm = TRUE)
      }
    }
    if (nrow(active) > 0L && !any_fetch) {
      if (!io$release_exists()) stop("cold start but no counts fetched")
      return(heartbeat("all count fetches failed"))
    }

    counts_all <- if (length(counts_acc)) do.call(rbind, counts_acc) else
      data.frame(binary_name = character(0), version = character(0),
                 day = character(0), count = integer(0), stringsAsFactors = FALSE)
    daily_new <- aggregate_counts(counts_all, roster)
    # Change-gate: a frozen archive with no active releases produces nothing new.
    # But a cold start with no data must stop, never publish an empty heartbeat.
    if (nrow(daily_new) == 0L && !isTRUE(force_full)) {
      if (!io$release_exists()) stop("cold start but no download data was fetched")
      return(heartbeat("no new download data"))
    }
  }
  # reclassify-only rebuilds strictly from the already-downloaded shard history
  # (no re-crawl to merge in); every other path merges in the freshly-fetched tail.
  daily_all <- if (isTRUE(reclassify_only)) daily_hist else merge_daily(daily_hist, daily_new)
  if (nrow(daily_all) == 0L) {
    if (isTRUE(reclassify_only))
      stop("reclassify-only: no existing daily rows to rebuild the summary")
    return(heartbeat("no daily rows"))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  DBI::dbWriteTable(con, DAILY_TABLE, daily_all[c("package","date","count")], append = TRUE)

  # ENRICH the whole roster from the ledger: canonical_name / identity_state are
  # refreshed by routing every persisted binary_name through the one resolver.
  # Idempotent for origin (prefix-authoritative). Skipped on degrade so persisted
  # identity is never regressed; identity_state stays NA where the ledger is silent.
  if (!is.null(ledger) && nrow(roster) > 0L) {
    ident <- resolve_identities(roster$binary_name, ledger)
    ib <- match(roster$binary_name, ident$binary_name)
    ok <- !is.na(ib)
    roster$origin[ok]         <- ident$origin[ib[ok]]
    roster$canonical_name[ok] <- ident$canonical_name[ib[ok]]
    roster$identity_state[ok] <- ident$identity_state[ib[ok]]
  }

  anchor <- max(daily_all$date)
  summary_df <- build_summary(con, roster, anchor, prior_summary = prior_summary)

  changed <- character(0); shard_updates <- list()
  # reclassify-only touches no year: raw daily history is unchanged, so no year
  # shard is rewritten. Only the recent + summary shards (and the manifest)
  # carry the refreshed identity.
  touched <- if (isTRUE(reclassify_only)) character(0)
             else if (isTRUE(force_full)) sort(unique(substr(daily_all$date, 1, 4)))
             else sort(unique(substr(daily_new$date, 1, 4)))
  for (yr in touched) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    rows <- extract_year(con, as.integer(yr))
    export_shard(file.path(out_dir, f), rows)
    changed <- c(changed, f); shard_updates[[f]] <- coverage(rows)
  }
  r_rows <- extract_recent(con, today, RECENT_WINDOW_DAYS)
  export_shard(recent_path, r_rows)
  embed_aux(recent_path, summary_df, roster)
  export_summary_shard(summary_path, summary_df)
  changed <- c(changed, basename(recent_path), basename(summary_path))
  shard_updates[[basename(recent_path)]] <- coverage(r_rows)

  enabled_keys <- vapply(Filter(function(a) isTRUE(a$enabled), ARCHIVES),
                         function(a) a$key, character(1))
  out <- list(
    tag = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(now), last_checked = iso(now), last_changed = iso(now),
    source_kind = if (isTRUE(reclassify_only)) "reclassify" else "launchpad",
    archives = as.list(enabled_keys),
    changed_shards = as.list(changed),
    shards = merge_shard_coverage(prev_shards, shard_updates),
    summary = list(packages = nrow(summary_df), latest_date = anchor,
                   releases = nrow(roster), active_releases = nrow(active)))
  write_manifest(manifest_path, out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  list(changed_shards = changed, manifest = out)
}

default_io <- function() {
  gh_rc <- function(args) {
    st <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
    as.integer(attr(st, "status") %||% 0L)
  }
  list(
    release_exists = function() identical(gh_rc(
      c("release", "view", "current", "--repo", PUBLISH_REPO)), 0L),
    release_download = function(pattern, dir) {
      for (i in seq_len(3L)) {
        code <- gh_rc(c("release", "download", "current", "--repo", PUBLISH_REPO,
                        "--pattern", pattern, "--dir", dir, "--clobber"))
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      1L
    },
    fetch = function(url) {
      tryCatch(with_retry({
        h <- curl::new_handle(useragent = USER_AGENT, timeout = 90L, connecttimeout = 20L)
        r <- curl::curl_fetch_memory(url, handle = h)
        if (r$status_code != 200L) stop("HTTP ", r$status_code)
        rawToChar(r$content)
      }), error = function(e) NULL)
    },
    # Concurrent multi-url fetch for the sharded backfill. Pool size is capped by
    # config POOL but overridable per job via C2D4U_POOL so the workflow can hold
    # max-parallel * POOL under Launchpad's ~24-connection throttle.
    fetch_many = function(urls) {
      pool <- suppressWarnings(as.integer(Sys.getenv("C2D4U_POOL", as.character(POOL))))
      if (is.na(pool) || pool < 1L) pool <- POOL
      fetch_pool(urls, pool = pool)
    },
    cran_names = function() rownames(utils::available.packages(repos = CRAN_REPO)),
    archive_names = function() parse_archive_index(fetch_pool(CRAN_ARCHIVE_INDEX)[[1]]),
    bioc_names = function() {
      urls <- sprintf("%s/%s/VIEWS", BIOC_VIEWS_BASE, BIOC_VIEWS_CATEGORIES)
      unique(unlist(lapply(urls, function(u) {
        txt <- tryCatch(rawToChar(curl::curl_fetch_memory(u)$content), error = function(e) "")
        parse_views_packages(txt)
      }), use.names = FALSE))
    },
    # Downloads the shared identity assets (cran-archive's cran_names_all and
    # bioconductor-metadata's bioc_names_all) from each source repo's `current`
    # release into a temp dir, for robservatory::load_identity.
    identity_dbs = function() {
      tmp <- tempfile(); dir.create(tmp, showWarnings = FALSE)
      dl <- function(repo, db) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", repo,
            "--pattern", db, "--dir", tmp, "--clobber"), stdout = FALSE, stderr = FALSE))
        p <- file.path(tmp, db)
        if (!identical(as.integer(st), 0L) || !file.exists(p)) stop("identity asset unreachable: ", repo, "/", db)
        p
      }
      list(cran = dl(CRAN_ARCHIVE_REPO, CRAN_ARCHIVE_DB),
           bioc = dl(BIOC_META_REPO, BIOC_META_DB))
    },
    now = function() Sys.time())
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[[1]] else "out"
  reclassify_only <- tolower(Sys.getenv(RECLASSIFY_ONLY_ENV, "")) %in% c("true", "1", "yes")
  # force_full and reclassify_only are independent env flags, but reclassify_only
  # is the cheaper, no-Launchpad path -- if both are set, it wins and force_full
  # is ignored so the two are never both effectively active.
  force_full <- !reclassify_only &&
    tolower(Sys.getenv(FORCE_REBUILD_ENV, "")) %in% c("true", "1", "yes")
  res <- run_update(default_io(), out_dir, force_full, reclassify_only)
  cat("changed shards:", paste(res$changed_shards, collapse = ", "), "\n")
}
