`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Build the lowercased-token -> canonical-name and -> identity-state maps from
# the org identity ledger. Replaces the former live available.packages()/VIEWS
# fetch: the ledger is append-only (covers archived packages the live index has
# dropped) and size-gated by the caller. robservatory has already applied
# cran>bioc precedence into $lookup, so a single merged map is correct. Returns
# the maps plus table sizes so the caller can size-gate before trusting them.
build_identity_maps <- function(cran_db_path, bioc_db_path) {
  maps <- robservatory::load_identity(cran_db_path, bioc_db_path)
  lk   <- maps$lookup
  tokens <- ls(lk)
  canon <- character(length(tokens)); state <- character(length(tokens))
  for (i in seq_along(tokens)) {
    r <- get(tokens[i], envir = lk)
    canon[i] <- r$canonical_name
    state[i] <- r$identity_state
  }
  list(
    name_map  = stats::setNames(canon, tokens),
    state_map = stats::setNames(state, tokens),
    n_cran    = maps$n_cran,
    n_bioc    = maps$n_bioc)
}

# The degrade resolver: every lookup misses, so cran/bioc tokens fall back to the
# token and identity_state is NA. Used when the ledger is unreachable or fails
# the size gate on the live path, so a run never drops a row or fabricates state.
empty_identity_maps <- function() list(
  name_map  = stats::setNames(character(0), character(0)),
  state_map = stats::setNames(character(0), character(0)),
  n_cran = 0L, n_bioc = 0L)

# Download the identity assets via io$identity_dbs(), build the maps, and
# size-gate both tables. Errors (asset unreachable or a failed gate) propagate so
# the caller decides whether to degrade (live monthly) or abort (bootstrap).
load_gated_maps <- function(io, live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  dbs  <- io$identity_dbs()
  maps <- build_identity_maps(dbs$cran, dbs$bioc)
  if (!robservatory::check_size(maps$n_cran, floor = live_floor) ||
      !robservatory::check_size(maps$n_bioc, floor = bioc_floor))
    stop("identity size gate failed (cran=", maps$n_cran, ", bioc=", maps$n_bioc, ")")
  maps
}

lp_archive_ref <- function(archive) {
  sprintf("%s/~%s/+archive/ubuntu/%s",
          LP_API_BASE, archive$owner, utils::URLencode(archive$ref, reserved = TRUE))
}

lp_published_url <- function(archive, start = 0L, size = PAGE_SIZE, status = NULL) {
  # ordered=false is REQUIRED: the default ordered=true forces an expensive
  # server-side sort that intermittently returns HTTP 503 on deep offsets
  # (past ~12,900 entries), which makes the ~326k-entry whole-archive sweep
  # impossible. ordered=false pages reliably at any depth and ~3x faster; order
  # is irrelevant since we page the complete set and dedupe. Launchpad echoes
  # the param into next_collection_link, so every paged request keeps it.
  u <- sprintf("%s?ws.op=getPublishedBinaries&ordered=false&ws.size=%d&ws.start=%d",
               lp_archive_ref(archive), as.integer(size), as.integer(start))
  if (!is.null(status)) u <- paste0(u, "&status=", status)
  u
}

lp_counts_url <- function(archive, pub_id, start_date = NULL, size = PAGE_SIZE) {
  u <- sprintf("%s/+binarypub/%d?ws.op=getDownloadCounts&ws.size=%d",
               lp_archive_ref(archive), as.integer(pub_id), as.integer(size))
  if (!is.null(start_date)) u <- paste0(u, "&start_date=", start_date)
  u
}

lp_pub_id <- function(self_link) {
  as.integer(sub(".*/\\+binarypub/([0-9]+).*$", "\\1", self_link))
}

parse_arch <- function(distro_arch_series_link) {
  ifelse(is.na(distro_arch_series_link), NA_character_,
         basename(distro_arch_series_link))
}

parse_published_page <- function(txt) {
  j <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  e <- j$entries
  if (is.null(e) || length(e) == 0L || (is.data.frame(e) && nrow(e) == 0L)) {
    entries <- data.frame(pub_id = integer(0), binary_name = character(0),
                          version = character(0), arch = character(0),
                          status = character(0), date_published = character(0),
                          stringsAsFactors = FALSE)
  } else {
    entries <- data.frame(
      pub_id         = lp_pub_id(e$self_link),
      binary_name    = as.character(e$binary_package_name),
      version        = as.character(e$binary_package_version),
      arch           = parse_arch(e$distro_arch_series_link),
      status         = as.character(e$status),
      date_published = as.character(e$date_published),
      stringsAsFactors = FALSE)
  }
  nl <- j$next_collection_link
  list(entries = entries, next_link = if (is.null(nl)) NA_character_ else as.character(nl))
}

parse_counts_page <- function(txt) {
  j <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  e <- j$entries
  if (is.null(e) || length(e) == 0L || (is.data.frame(e) && nrow(e) == 0L)) {
    rows <- data.frame(binary_name = character(0), version = character(0),
                       day = character(0), count = integer(0), stringsAsFactors = FALSE)
  } else {
    rows <- data.frame(
      binary_name = as.character(e$binary_package_name),
      version     = as.character(e$binary_package_version),
      day         = as.character(e$day),
      count       = as.integer(e$count),
      stringsAsFactors = FALSE)
  }
  nl <- j$next_collection_link
  list(rows = rows, next_link = if (is.null(nl)) NA_character_ else as.character(nl))
}

parse_views_packages <- function(views_text) {
  lines <- unlist(strsplit(views_text, "\n", fixed = TRUE))
  hits <- grep("^Package:\\s*", lines, value = TRUE)
  trimws(sub("^Package:\\s*", "", hits))
}

# Prefix-authoritative origin for c2d4u. Non-r-* names are dropped. canonical_name
# and identity_state come from the org identity ledger (`maps$name_map` /
# `maps$state_map`, keyed by the lowercased token). A cran/bioc token absent from
# the ledger keeps canonical = token and identity_state = NA (honest unknown);
# origin='other' keeps canonical = NA and identity_state = NA (off the leaderboard).
resolve_identities <- function(binary_names, maps) {
  name_map  <- maps$name_map  %||% stats::setNames(character(0), character(0))
  state_map <- maps$state_map %||% stats::setNames(character(0), character(0))

  bn <- unique(binary_names)
  pref <- rep(NA_character_, length(bn))
  pref[startsWith(bn, "r-cran-")]  <- "cran"
  pref[startsWith(bn, "r-bioc-")]  <- "bioc"
  pref[startsWith(bn, "r-other-")] <- "other"
  keep <- !is.na(pref)
  bn <- bn[keep]; pref <- pref[keep]
  token <- tolower(sub("^r-(cran|bioc|other)-", "", bn))

  canonical <- rep(NA_character_, length(bn))
  state     <- rep(NA_character_, length(bn))
  scoped <- pref %in% c("cran", "bioc")   # cran/bioc look up the ledger; other stays NA
  if (any(scoped)) {
    mapped <- unname(name_map[token[scoped]])
    canonical[scoped] <- ifelse(is.na(mapped), token[scoped], mapped)
    state[scoped]     <- unname(state_map[token[scoped]])
  }

  df <- data.frame(binary_name = bn, package = token, origin = pref,
                   canonical_name = canonical, identity_state = state,
                   stringsAsFactors = FALSE)
  # Keep one row per package token, preferring cran > bioc > other, preserving
  # first-appearance order (a plain order()+!duplicated() would sort alphabetically
  # and break callers that rely on input order).
  rankv <- match(df$origin, c("cran", "bioc", "other"))
  unique_tokens <- unique(df$package)
  keep_rows <- integer(0)
  for (t in unique_tokens) {
    mask <- df$package == t
    best_idx <- which(mask)[which.min(rankv[mask])]
    keep_rows <- c(keep_rows, best_idx)
  }
  df <- df[keep_rows, , drop = FALSE]
  rownames(df) <- NULL
  df
}

aggregate_counts <- function(counts_df, identity_df) {
  empty <- data.frame(package = character(0), date = character(0),
                      count = integer(0), stringsAsFactors = FALSE)
  if (nrow(counts_df) == 0L) return(empty)
  pkg <- identity_df$package[match(counts_df$binary_name, identity_df$binary_name)]
  keep <- !is.na(pkg)
  if (!any(keep)) return(empty)
  df <- data.frame(package = pkg[keep], date = counts_df$day[keep],
                   count = as.integer(counts_df$count[keep]), stringsAsFactors = FALSE)
  bad <- is.na(df$date) | is.na(df$count)
  if (any(bad)) {
    warning(sprintf("aggregate_counts: dropping %d row(s) with NA day/count", sum(bad)))
    df <- df[!bad, , drop = FALSE]
  }
  if (nrow(df) == 0L) return(empty)
  agg <- stats::aggregate(count ~ package + date, data = df, FUN = sum)
  agg <- agg[order(agg$package, agg$date), , drop = FALSE]
  agg$count <- as.integer(agg$count)
  rownames(agg) <- NULL
  agg
}

merge_daily <- function(old_df, new_df) {
  out <- rbind(old_df, new_df)
  out <- out[order(out$package, out$date), , drop = FALSE]
  key <- paste(out$package, out$date)
  # On a (package,date) conflict new_df wins because it is appended last;
  # keep the LAST occurrence of each key. Rows only in old_df are preserved.
  out <- out[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

daily_table_ddl <- function(table) sprintf(
  "CREATE TABLE %s (
     package TEXT    NOT NULL,
     date    TEXT    NOT NULL,
     count   INTEGER NOT NULL,
     PRIMARY KEY (package, date))", table)

summary_table_ddl <- function(table) sprintf(
  "CREATE TABLE %s (
     package       TEXT,
     package_lower TEXT,
     origin        TEXT,
     canonical_name TEXT,
     total_30d     INTEGER,
     total_90d     INTEGER,
     total_365d    INTEGER,
     rank_30d      INTEGER,
     rank_90d      INTEGER,
     rank_365d     INTEGER,
     avg_daily_30d REAL,
     trend         REAL,
     first_date    TEXT,
     last_date     TEXT,
     cnt_total     INTEGER,
     identity_state TEXT,
     PRIMARY KEY (package))", table)

releases_table_ddl <- function(table) sprintf(
  "CREATE TABLE %s (
     archive        TEXT,
     binary_name    TEXT,
     version        TEXT,
     pub_id         INTEGER,
     package        TEXT,
     origin         TEXT,
     canonical_name TEXT,
     identity_state TEXT,
     cnt_total      INTEGER,
     last_day       TEXT,
     done           INTEGER,
     PRIMARY KEY (archive, binary_name, version))", table)

export_shard <- function(path, daily_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  DBI::dbExecute(con, sprintf("CREATE INDEX idx_c2_date ON %s(date)", DAILY_TABLE))
  if (nrow(daily_df) > 0)
    DBI::dbWriteTable(con, DAILY_TABLE, daily_df[c("package","date","count")], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

export_summary_shard <- function(path, summary_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, summary_table_ddl(SUMMARY_TABLE))
  if (nrow(summary_df) > 0)
    DBI::dbWriteTable(con, SUMMARY_TABLE, summary_df[SUMMARY_COLS], append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

shard_key <- function(package) {
  c1 <- substr(tolower(package), 1L, 1L)
  ifelse(grepl("^[a-z]$", c1), c1, "0")
}

extract_year <- function(con, year) {
  DBI::dbGetQuery(con, sprintf(
    "SELECT package,date,count FROM %s WHERE substr(date,1,4)='%04d'", DAILY_TABLE, as.integer(year)))
}

extract_recent <- function(con, anchor_date, window_days) {
  cut <- format(as.Date(anchor_date) - window_days, "%Y-%m-%d")
  DBI::dbGetQuery(con, sprintf(
    "SELECT package,date,count FROM %s WHERE date >= '%s'", DAILY_TABLE, cut))
}

empty_summary <- function() {
  data.frame(package = character(0), package_lower = character(0),
             origin = character(0), canonical_name = character(0),
             total_30d = integer(0), total_90d = integer(0), total_365d = integer(0),
             rank_30d = integer(0), rank_90d = integer(0), rank_365d = integer(0),
             avg_daily_30d = numeric(0), trend = numeric(0),
             first_date = character(0), last_date = character(0),
             cnt_total = integer(0), identity_state = character(0),
             stringsAsFactors = FALSE)
}

build_summary <- function(daily_con, identity_df, anchor_date, prior_summary = NULL) {
  a <- format(as.Date(anchor_date), "%Y-%m-%d")
  agg <- DBI::dbGetQuery(daily_con, sprintf("
    SELECT package,
      MIN(date) AS first_date, MAX(date) AS last_date, SUM(count) AS cnt_total,
      SUM(CASE WHEN date >= date('%1$s','-30 days')  THEN count ELSE 0 END) AS total_30d,
      SUM(CASE WHEN date >= date('%1$s','-90 days')  THEN count ELSE 0 END) AS total_90d,
      SUM(CASE WHEN date >= date('%1$s','-365 days') THEN count ELSE 0 END) AS total_365d,
      SUM(CASE WHEN date >  date('%1$s','-60 days')
                AND date <  date('%1$s','-30 days') THEN count ELSE 0 END) AS prev_30d
    FROM %2$s GROUP BY package", a, DAILY_TABLE))

  if (nrow(agg) == 0L && is.null(prior_summary)) return(empty_summary())

  agg$package_lower <- tolower(agg$package)
  agg$avg_daily_30d <- round(agg$total_30d / 30, 2)
  agg$trend <- ifelse(!is.na(agg$prev_30d) & agg$prev_30d > 0,
                      round((agg$total_30d / agg$prev_30d - 1) * 100, 2), NA_real_)
  # The roster has one row per (binary,version); collapse to one identity per
  # package token before joining so the aggregate is not fanned out.
  id1 <- identity_df[!duplicated(identity_df$package),
                     c("package","origin","canonical_name","identity_state")]
  agg <- merge(agg, id1, by = "package", all.x = TRUE)
  agg$origin <- ifelse(is.na(agg$origin), "other", agg$origin)

  for (col in c("total_30d","total_90d","total_365d","cnt_total"))
    agg[[col]] <- as.integer(agg[[col]])

  cur <- agg[c("package","package_lower","origin","canonical_name",
               "total_30d","total_90d","total_365d","avg_daily_30d","trend",
               "first_date","last_date","cnt_total","identity_state")]

  # Merge-forward: prior packages absent this run keep identity + first_date +
  # last_date + cnt_total, with zeroed current windows.
  if (!is.null(prior_summary) && nrow(prior_summary) > 0) {
    gone <- prior_summary[!prior_summary$package %in% cur$package, , drop = FALSE]
    if (nrow(gone) > 0) {
      carry <- data.frame(
        package = gone$package, package_lower = gone$package_lower,
        origin = gone$origin, canonical_name = gone$canonical_name,
        total_30d = 0L, total_90d = 0L, total_365d = 0L,
        avg_daily_30d = 0, trend = NA_real_,
        first_date = gone$first_date, last_date = gone$last_date,
        cnt_total = as.integer(gone$cnt_total),
        identity_state = gone$identity_state, stringsAsFactors = FALSE)
      cur <- rbind(cur, carry)
    }
    # Preserve the earliest first_date ever seen for surviving packages.
    fd <- prior_summary$first_date[match(cur$package, prior_summary$package)]
    cur$first_date <- pmin(cur$first_date, ifelse(is.na(fd), cur$first_date, fd))
  }

  cur$rank_30d  <- as.integer(rank(-cur$total_30d,  ties.method = "min"))
  cur$rank_90d  <- as.integer(rank(-cur$total_90d,  ties.method = "min"))
  cur$rank_365d <- as.integer(rank(-cur$total_365d, ties.method = "min"))
  cur <- cur[order(cur$rank_30d, cur$package), , drop = FALSE]
  rownames(cur) <- NULL
  cur[SUMMARY_COLS]
}

iso <- function(t) format(as.POSIXct(t), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

coverage <- function(rows) {
  valid <- rows$date[!is.na(rows$date)]
  if (length(valid) == 0L)
    return(list(rows = nrow(rows), date_min = NA_character_, date_max = NA_character_))
  list(rows = nrow(rows), date_min = min(valid), date_max = max(valid))
}

merge_shard_coverage <- function(prev, updates) {
  out <- prev %||% list()
  for (k in names(updates)) out[[k]] <- updates[[k]]
  out
}

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  — present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    — macOS/local fallback
#' No heavy dependency is declared: on CI (which installs only RSQLite,
#' jsonlite, testthat, DBI) the coreutils `sha256sum` path is used. If a
#' sibling pipeline already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path` (call this only after the file is finalized):
#'   * db_filename — basename of the file
#'   * db_bytes    — integer byte size of the file
#'   * db_sha256   — lowercase hex sha256 of the file's exact bytes
#'   * tables      — named list mapping each user table to its row count
#'   * complete    — passed through by the caller (TRUE for a full rebuild)
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tbl_names <- DBI::dbGetQuery(con, "
    SELECT name FROM sqlite_master
     WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
     ORDER BY name")$name

  tables <- stats::setNames(
    lapply(tbl_names, function(t) {
      DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
    }),
    tbl_names
  )

  list(
    db_filename = basename(db_path),
    db_bytes    = as.integer(file.size(db_path)),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

#' Serialize the manifest object to JSON.
#'
#' `core` (optional) is a named list of TOP-LEVEL fields to merge into the
#' manifest — used to attach the integrity/completeness core built by
#' summary_integrity_core() (db_filename, db_bytes, db_sha256, tables, complete).
write_manifest <- function(path, obj, core = NULL) {
  if (!is.null(core)) {
    obj <- c(obj, core)  # merge as top-level fields, not nested
  }
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
}

write_release_notes <- function(path, manifest) {
  or_na <- function(x) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) "n/a" else as.character(x)
  ts    <- function(s) if (is.null(s) || length(s) == 0 || is.na(s)) "n/a" else sub("Z$", " UTC", sub("T", " ", s))
  cs   <- manifest$changed_shards
  chng <- if (length(cs) == 0) "none (no change since last run)" else paste(unlist(cs), collapse = ", ")
  sm   <- manifest$summary %||% list()

  lines <- c(
    "## c2d4u Downloads (rolling)",
    "",
    "Per-package daily download counts for CRAN and Bioconductor R packages",
    "distributed as Ubuntu .debs through the Launchpad c2d4u PPAs.",
    "This is a frozen legacy channel (see the repository README).",
    "",
    "| field | value |",
    "| --- | --- |",
    sprintf("| last checked | %s |", ts(manifest$last_checked)),
    sprintf("| last changed | %s |", ts(manifest$last_changed)),
    sprintf("| source | %s |", or_na(manifest$source_kind)),
    sprintf("| packages | %s |", or_na(sm$packages)),
    sprintf("| latest data day | %s |", or_na(sm$latest_date)),
    sprintf("| changed this run | %s |", chng),
    "",
    "## Shard coverage",
    "",
    "| shard | rows | from | to |",
    "| --- | --- | --- | --- |")
  for (nm in sort(names(manifest$shards))) {
    s <- manifest$shards[[nm]]
    lines <- c(lines, sprintf("| %s | %s | %s | %s |",
                              nm, or_na(s$rows), or_na(s$date_min), or_na(s$date_max)))
  }
  lines <- c(lines, "", "## Download", "",
             "```sh",
             "gh release download current --repo r-observatory/c2d4u-downloads \\",
             "  --pattern 'c2d4u-downloads-recent.db'",
             "```")
  writeLines(lines, path)
}

embed_aux <- function(recent_path, summary_df, releases_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", SUMMARY_TABLE))
  DBI::dbExecute(con, summary_table_ddl(SUMMARY_TABLE))
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, SUMMARY_TABLE, summary_df[SUMMARY_COLS], append = TRUE)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", RELEASES_TABLE))
  DBI::dbExecute(con, releases_table_ddl(RELEASES_TABLE))
  if (nrow(releases_df) > 0)
    DBI::dbWriteTable(con, RELEASES_TABLE,
      releases_df[c("archive","binary_name","version","pub_id","package",
                    "origin","canonical_name","identity_state","cnt_total","last_day","done")],
      append = TRUE)
  invisible(NULL)
}

.empty_releases <- function() {
  data.frame(archive = character(0), binary_name = character(0), version = character(0),
             pub_id = integer(0), package = character(0), origin = character(0),
             canonical_name = character(0), identity_state = character(0),
             cnt_total = integer(0), last_day = character(0), done = integer(0),
             stringsAsFactors = FALSE)
}

load_releases <- function(path) {
  if (!file.exists(path)) return(.empty_releases())
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!RELEASES_TABLE %in% DBI::dbListTables(con)) return(.empty_releases())
  df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", RELEASES_TABLE))
  if (!"identity_state" %in% names(df)) df$identity_state <- rep(NA_character_, nrow(df))
  df
}

paginate <- function(fetch, first_url, parse_fn, field) {
  acc <- list(); url <- first_url; guard <- 0L
  while (length(url) == 1L && !is.na(url)) {
    guard <- guard + 1L
    if (guard > 100000L) stop("paginate: runaway paging")
    txt <- fetch(url)
    if (is.null(txt)) stop("paginate: fetch failed for ", url)
    pr <- parse_fn(txt)
    acc[[length(acc) + 1L]] <- pr[[field]]
    url <- pr$next_link
  }
  if (length(acc) == 0L) return(NULL)
  do.call(rbind, acc)
}

# ---------------------------------------------------------------------------
# CONCURRENCY POOL (backfill). Fetch every url with a bounded curl::multi pool,
# then make several retry passes over the failed/NULL indices with growing
# sleeps so a 503 wave is ridden out rather than dropping data. Returns a list
# aligned to `urls`: the response body string on HTTP 200, NULL otherwise.
fetch_pool <- function(urls, pool = POOL, passes = FETCH_PASSES, block = 1500L) {
  out <- vector("list", length(urls))
  n <- length(urls)
  if (n == 0L) return(out)
  run <- function(idxs) {
    for (s in seq(1L, length(idxs), by = block)) {
      e   <- min(s + block - 1L, length(idxs))
      sel <- idxs[s:e]
      p   <- curl::new_pool(total_con = pool, host_con = pool)
      for (j in sel) {
        local({
          jj <- j
          h <- curl::new_handle(useragent = USER_AGENT, timeout = 90L, connecttimeout = 20L)
          curl::handle_setopt(h, url = urls[jj])
          curl::multi_add(h,
            done = function(res) if (isTRUE(res$status_code == 200L)) out[[jj]] <<- rawToChar(res$content),
            fail = function(err) invisible(NULL),
            pool = p)
        })
      }
      curl::multi_run(pool = p)
    }
  }
  run(seq_len(n))
  for (k in seq_len(passes - 1L)) {
    failed <- which(vapply(out, is.null, logical(1)))
    if (length(failed) == 0L) break
    Sys.sleep(3 * k)   # growing backoff between passes
    run(failed)
  }
  out
}

# Fetch a set of first-page urls through `fetch_many` (a urls -> list-of-bodies
# function, e.g. fetch_pool or a test fake), parse each with parse_fn, and follow
# next_collection_link concurrently until every item is exhausted. Only a handful
# of names/releases exceed one page, so later waves shrink quickly. Returns
# list(data = per-item field data.frame or partial/NULL, ok = logical: TRUE only
# where the item fully completed with no failed page).
fetch_paginated <- function(fetch_many, first_urls, parse_fn, field) {
  n <- length(first_urls)
  acc <- vector("list", n)         # accumulated field rows per item
  ok  <- logical(n)                # settled-complete flag per item
  cur <- first_urls                # current url to fetch per item
  active <- seq_len(n)
  guard <- 0L
  while (length(active) > 0L) {
    guard <- guard + 1L
    if (guard > 100000L) stop("fetch_paginated: runaway paging")
    bodies <- fetch_many(cur[active])
    nxt <- integer(0)
    for (m in seq_along(active)) {
      i <- active[m]; body <- bodies[[m]]
      if (is.null(body)) next          # failed page -> item stays ok=FALSE
      pr <- tryCatch(parse_fn(body), error = function(e) NULL)
      if (is.null(pr)) next
      acc[[i]] <- if (is.null(acc[[i]])) pr[[field]] else rbind(acc[[i]], pr[[field]])
      nl <- pr$next_link
      if (length(nl) == 1L && !is.na(nl)) { cur[i] <- nl; nxt <- c(nxt, i) }
      else ok[i] <- TRUE
    }
    active <- nxt
  }
  list(data = acc, ok = ok)
}

# ---------------------------------------------------------------------------
# NAME-LIST ENUMERATION. The whole-archive getPublishedBinaries sweep is
# impossible (Launchpad 503s past ~12,900 entries). Instead enumerate the roster
# with cheap, reliable per-package-name filtered queries.

# The per-name filtered query. ordered=false + exact_match=true is a cheap index
# lookup (~1s, no 503s), unlike the ordered deep-offset whole-archive sweep.
lp_name_query_url <- function(archive, binary_name, size = PAGE_SIZE) {
  sprintf("%s?ws.op=getPublishedBinaries&binary_name=%s&exact_match=true&ordered=false&ws.size=%d",
          lp_archive_ref(archive), curl::curl_escape(binary_name), as.integer(size))
}

# Parse the CRAN src/contrib/Archive/ directory listing into package names.
parse_archive_index <- function(html) {
  if (is.null(html) || !nzchar(html)) return(character(0))
  m <- regmatches(html, gregexpr('href="([^"/]+)/"', html))[[1]]
  nm <- sub('href="([^"/]+)/"', "\\1", m)
  nm[!nm %in% c("..", ".") & nzchar(nm)]
}

# The candidate binary-name universe: current CRAN + every ever-archived CRAN
# package -> r-cran-<lower>, and Bioc VIEWS packages -> r-bioc-<lower>.
candidate_binary_names <- function(cran_names, archive_names, bioc_names = character(0)) {
  clean <- function(x) { x <- x[!is.na(x)]; unique(x[nzchar(x)]) }
  # paste0() recycles a zero-length arg to "", so only prefix non-empty vectors.
  pref  <- function(p, x) if (length(x)) paste0(p, tolower(x)) else character(0)
  # The only r-other- packages (non-CRAN/non-Bioc extras Rutter hand-packaged),
  # all in ~marutter/c2d4u3.5; enumerated live 2026-07-05. They are not in the
  # CRAN/Bioc name lists, so add them explicitly (origin=other, canonical=NA).
  r_other <- c("amsmercury", "curvefdp", "hms-dbmi-spp", "iwrlars", "nitpick")
  unique(c(pref("r-cran-", clean(c(cran_names, archive_names))),
           pref("r-bioc-", clean(bioc_names)),
           pref("r-other-", r_other)))
}

# Enumerate every candidate name for one archive via the per-name filtered query
# through `fetch_many` (concurrent). Names that 503 or 404 simply contribute no
# rows. Returns entries tagged with the archive key (empty-with-archive if none).
enumerate_names <- function(fetch_many, candidates, archive, batch = ENUM_BATCH) {
  empty <- cbind(archive = character(0),
    data.frame(pub_id = integer(0), binary_name = character(0),
               version = character(0), arch = character(0),
               status = character(0), date_published = character(0),
               stringsAsFactors = FALSE))
  if (length(candidates) == 0L) return(empty)
  acc <- list()
  batches <- split(candidates, ceiling(seq_along(candidates) / batch))
  for (nm in batches) {
    urls <- vapply(nm, function(x) lp_name_query_url(archive, x), character(1))
    res <- fetch_paginated(fetch_many, urls, parse_published_page, "entries")
    got <- res$ok
    if (any(got)) {
      ent_list <- res$data[got]
      ent <- do.call(rbind, ent_list[!vapply(ent_list, is.null, logical(1))])
      if (!is.null(ent) && nrow(ent) > 0L) acc[[length(acc) + 1L]] <- ent
    }
  }
  if (length(acc) == 0L) return(empty)
  cbind(archive = archive$key, do.call(rbind, acc), stringsAsFactors = FALSE)
}

# EVEN sharding by row index modulo N (first-letter buckets are very uneven).
# Shard i owns roster rows where ((rownumber - 1) %% N) == i.
shard_rows <- function(n, i, N) {
  if (n == 0L || N <= 0L) return(integer(0))
  which(((seq_len(n) - 1L) %% N) == i)
}

enumerate_archive <- function(fetch, archive) {
  ent <- paginate(fetch, lp_published_url(archive), parse_published_page, "entries")
  if (is.null(ent) || nrow(ent) == 0L) {
    return(cbind(archive = character(0),
                 data.frame(pub_id = integer(0), binary_name = character(0),
                            version = character(0), arch = character(0),
                            status = character(0), date_published = character(0),
                            stringsAsFactors = FALSE)))
  }
  cbind(archive = archive$key, ent, stringsAsFactors = FALSE)
}

dedup_releases <- function(entries) {
  if (nrow(entries) == 0L) return(entries)
  ord <- order(entries$archive, entries$binary_name, entries$version,
               entries$arch != "amd64")   # amd64 sorts first
  e <- entries[ord, , drop = FALSE]
  key <- paste(e$archive, e$binary_name, e$version)
  e <- e[!duplicated(key), , drop = FALSE]
  rownames(e) <- NULL
  e
}

build_roster <- function(entries, maps) {
  d <- dedup_releases(entries)
  if (nrow(d) == 0L) return(.empty_releases())
  ident <- resolve_identities(d$binary_name, maps)  # drops toolchain, one origin per token
  # Note: resolve_identities keeps one binary per package token (cran > bioc >
  # other). If the same token exists under two prefixes (rare; CRAN/Bioc names
  # are mostly disjoint), the losing binary is dropped and its counts are not
  # summed under that token.
  ib <- match(d$binary_name, ident$binary_name)
  keep <- !is.na(ib)
  d <- d[keep, , drop = FALSE]; ib <- ib[keep]
  data.frame(
    archive = d$archive, binary_name = d$binary_name, version = d$version,
    pub_id = as.integer(d$pub_id), package = ident$package[ib],
    origin = ident$origin[ib], canonical_name = ident$canonical_name[ib],
    identity_state = ident$identity_state[ib],
    cnt_total = NA_integer_, last_day = NA_character_, done = 0L,
    stringsAsFactors = FALSE)
}

archive_by_key <- function(key) {
  for (a in ARCHIVES) if (identical(a$key, key)) return(a)
  NULL
}

load_daily <- function(path) {
  empty <- data.frame(package = character(0), date = character(0),
                      count = integer(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DAILY_TABLE %in% DBI::dbListTables(con)) return(empty)
  DBI::dbGetQuery(con, sprintf("SELECT package,date,count FROM %s", DAILY_TABLE))
}

load_summary <- function(path) {
  if (!file.exists(path)) return(empty_summary())
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!SUMMARY_TABLE %in% DBI::dbListTables(con)) return(empty_summary())
  # An older shard published before identity_state existed lacks the column
  # in its schema entirely, so SELECT * over it simply omits it (0 rows or
  # many). Backfill length-safe: rep(NA, nrow(df)) rather than a scalar NA,
  # which errors ("replacement has 1 row, data has 0") on a 0-row frame.
  df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", SUMMARY_TABLE))
  for (col in SUMMARY_COLS) if (!col %in% names(df)) df[[col]] <- rep(NA_character_, nrow(df))
  df[SUMMARY_COLS]
}
