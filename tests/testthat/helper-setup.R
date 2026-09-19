# Auto-sourced by testthat. During test_dir() the working directory is
# tests/testthat, so the repo root is two levels up.
.c2_root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(.c2_root, "scripts", "config.R"))
source(file.path(.c2_root, "scripts", "helpers.R"))

.c2_update <- file.path(.c2_root, "scripts", "update.R")
if (file.exists(.c2_update)) source(.c2_update)
.c2_backfill <- file.path(.c2_root, "scripts", "backfill.R")
if (file.exists(.c2_backfill)) source(.c2_backfill)

fixture_path <- function(...) {
  file.path(.c2_root, "tests", "testthat", "fixtures", ...)
}

# Build a pair of ledger fixture DBs (cran_names_all / bioc_names_all) for the
# robservatory identity loader. `cran`/`bioc` are named character vectors:
# name_lower -> canonical; `states` (optional) name_lower -> live|archived.
mk_ledger_dbs <- function(dir, cran = character(0), bioc = character(0),
                          states = character(0)) {
  st <- function(k) { s <- unname(states[k]); ifelse(is.na(s), "live", s) }
  write_one <- function(path, table, vals) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT,
         identity_state TEXT, first_seen TEXT, last_seen TEXT)", table))
    if (length(vals) > 0L)
      DBI::dbWriteTable(con, table, data.frame(
        name_lower = names(vals), canonical_name = unname(vals),
        identity_state = st(names(vals)), first_seen = "x", last_seen = "y",
        stringsAsFactors = FALSE), append = TRUE)
  }
  cp <- file.path(dir, "cran-archive.db"); bp <- file.path(dir, "bioc-meta.db")
  write_one(cp, "cran_names_all", cran)
  write_one(bp, "bioc_names_all", bioc)
  list(cran = cp, bioc = bp)
}

# A hand-built ledger maps list (bypasses robservatory) for resolver unit tests:
# `name` is name_lower -> canonical, `state` is name_lower -> live|archived.
mk_maps <- function(name = character(0), state = character(0),
                    n_cran = length(name), n_bioc = 0L) {
  list(name_map = name, state_map = state, n_cran = n_cran, n_bioc = n_bioc)
}

# Stands in for default_io() with a `pub` temp dir playing the published
# `current` release. `pages` maps url -> body for the serial io$fetch and
# `pool_pages` (the same by default) answers the pooled io$fetch_many, so a test
# can fail a url in the pool and serve it to the serial residual pass. A url with
# no entry fails (NULL), as a failed request does. `now` is a POSIXct or a
# zero-argument clock function (for tests that move time forward).
fake_io <- function(pub, pages = list(), cran = character(0), bioc = character(0),
                    now = as.POSIXct("2026-07-04 00:00:00", tz = "UTC"),
                    ledger = NULL, fail_identity = FALSE, pool_pages = pages,
                    archive = character(0)) {
  clock <- if (is.function(now)) now else function() now
  list(
    release_exists = function() file.exists(file.path(pub, "manifest.json")),
    release_download = function(pattern, dir) {
      rx <- utils::glob2rx(pattern)
      hit <- list.files(pub, pattern = rx)
      if (length(hit) == 0) return(1L)
      for (h in hit) file.copy(file.path(pub, h), file.path(dir, h), overwrite = TRUE)
      0L
    },
    fetch = function(url) pages[[url]] %||% NULL,
    fetch_many = function(urls) lapply(urls, function(u) pool_pages[[u]] %||% NULL),
    cran_names = function() cran,
    archive_names = function() archive,
    bioc_names = function() bioc,
    identity_dbs = function() {
      if (isTRUE(fail_identity) || is.null(ledger)) stop("identity asset unreachable (test)")
      ledger
    },
    now = clock)
}

# fake_io that logs every Launchpad request: io$calls$fetch holds the serial
# urls in call order, io$calls$many one url vector per fetch_many call, and
# io$calls$deadlines the deadline each fetch_many call was given (its
# `deadline` argument makes fetch_paginated pass the deadline on).
recording_io <- function(...) {
  io <- fake_io(...)
  log <- new.env(parent = emptyenv())
  log$fetch <- character(0); log$many <- list(); log$deadlines <- list()
  one <- io$fetch; many <- io$fetch_many
  io$fetch <- function(url) { log$fetch <- c(log$fetch, url); one(url) }
  io$fetch_many <- function(urls, deadline = Inf) {
    log$many[[length(log$many) + 1L]] <- urls
    log$deadlines[[length(log$deadlines) + 1L]] <- deadline
    many(urls)
  }
  io$calls <- log
  io
}

# Copy a run's outputs into `pub`, as the workflow's publish step uploads them.
publish <- function(out, pub) {
  for (f in list.files(out, pattern = "\\.(db|json)$", full.names = TRUE))
    file.copy(f, file.path(pub, basename(f)), overwrite = TRUE)
}

# A getDownloadCounts page body for one (binary, version): one entry per day,
# newest first as Launchpad sends them, optionally linking a next page.
# total_size is the size of the whole collection, as Launchpad reports it on
# every page (by default this page's entries); start is the page's offset.
counts_json <- function(binary_name, version, day, count, next_link = NULL,
                        total_size = length(day), start = 0L) {
  entries <- data.frame(binary_package_name = rep(binary_name, length(day)),
                        binary_package_version = rep(version, length(day)),
                        day = as.character(day), count = as.integer(count),
                        stringsAsFactors = FALSE)
  as.character(jsonlite::toJSON(
    list(start = as.integer(start), total_size = as.integer(total_size),
         next_collection_link = next_link, entries = entries),
    auto_unbox = TRUE, null = "null"))
}

# Roster rows (c2d4u_releases columns) with test defaults: origin from the
# binary-name prefix, package token and canonical name from the rest, identity
# live, pub_id 1..n unless given.
mk_roster <- function(binary_name, version = "1.0", pub_id = seq_along(binary_name),
                      last_day = NA_character_, done = 1L, archive = "c2d4u4.0+") {
  n <- length(binary_name)
  pkg <- sub("^r-(cran|bioc|other)-", "", binary_name)
  data.frame(archive = rep_len(archive, n), binary_name = binary_name,
             version = rep_len(version, n), pub_id = as.integer(pub_id),
             package = pkg, origin = sub("^r-(cran|bioc|other)-.*$", "\\1", binary_name),
             canonical_name = pkg, identity_state = rep("live", n),
             cnt_total = rep(NA_integer_, n),
             last_day = rep_len(as.character(last_day), n),
             done = rep_len(as.integer(done), n), stringsAsFactors = FALSE)
}

# Write into `pub` a release exactly as a summed publish leaves it: a year shard
# per year of `daily` (package, date, count), the recent shard carrying `roster`
# and the summary, the summary DB, and a manifest built by contract_manifest, so
# it has history_method, counted_through and a fingerprint for every asset and
# passes verify_release. The summary is anchored on counted_through (default:
# manifest_extra$counted_through, else the last day of data), and each package
# in manifest_extra$package_edges on its own edge, as a publisher anchors it.
# unfetched_releases and empty_once_releases default to the roster's counts of
# done == 0 and done == 2. manifest_extra is
# then merged over the manifest with modifyList, so a test can drop a contract
# field (NULL), override one, or corrupt a fingerprint. Returns the manifest
# invisibly.
seed_release <- function(pub, roster, daily, manifest_extra = list(),
                         counted_through = manifest_extra$counted_through %||% max(daily$date),
                         now = as.POSIXct(paste(as.Date(counted_through) + SETTLED_LAG_DAYS,
                                                "01:00:00"), tz = "UTC")) {
  dir.create(pub, showWarnings = FALSE, recursive = TRUE)
  ct <- format(as.Date(counted_through), "%Y-%m-%d")
  daily <- daily[c("package", "date", "count")]
  if (any(daily$date > ct)) stop("seed_release: daily rows after counted_through ", ct)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(daily) > 0L) DBI::dbWriteTable(con, DAILY_TABLE, daily, append = TRUE)
  summary_df <- build_summary(con, roster, ct, edges = manifest_extra$package_edges)

  files <- character(0)
  for (yr in sort(unique(substr(daily$date, 1, 4)))) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    export_shard(file.path(pub, f), extract_year(con, as.integer(yr)))
    files <- c(files, f)
  }
  recent <- sprintf("%s-recent.db", SHARD_PREFIX)
  summ   <- sprintf("%s-summary.db", SHARD_PREFIX)
  export_shard(file.path(pub, recent), extract_recent(con, ct, RECENT_WINDOW_DAYS))
  embed_aux(file.path(pub, recent), summary_df, roster)
  export_summary_shard(file.path(pub, summ), summary_df)
  files <- c(files, recent, summ)

  keys <- vapply(Filter(function(a) isTRUE(a$enabled), ARCHIVES), function(a) a$key, character(1))
  base <- list(
    tag = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(now), last_checked = iso(now), last_changed = iso(now),
    source_kind = "launchpad", archives = as.list(keys),
    changed_shards = as.list(files), shards = list(),
    summary = list(packages = nrow(summary_df), latest_date = ct, releases = nrow(roster)))
  m <- contract_manifest(base, pub, files, ct,
    package_edges      = manifest_extra$package_edges %||% list(),
    unfetched_releases = manifest_extra$unfetched_releases %||% count_unfetched(roster$done),
    empty_once_releases = manifest_extra$empty_once_releases %||% count_empty_once(roster$done),
    detected_gaps      = manifest_extra$detected_gaps %||% list(),
    refetch_from       = manifest_extra$refetch_from,
    package_refetch_from = manifest_extra$package_refetch_from %||% list())
  m <- utils::modifyList(m, manifest_extra)
  write_manifest(file.path(pub, "manifest.json"), m)
  invisible(m)
}
