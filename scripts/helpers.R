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

# getDownloadCounts for one publication. start_date and end_date are both
# inclusive, and Launchpad keeps both in next_collection_link, so paging stays
# inside the window. Recent rows carry no per-country split (one row per day),
# so a window shorter than PAGE_SIZE days is normally a single page.
lp_counts_url <- function(archive, pub_id, start_date = NULL, end_date = NULL,
                          size = PAGE_SIZE) {
  u <- sprintf("%s/+binarypub/%d?ws.op=getDownloadCounts&ws.size=%d",
               lp_archive_ref(archive), as.integer(pub_id), as.integer(size))
  if (!is.null(start_date)) u <- paste0(u, "&start_date=", start_date)
  if (!is.null(end_date))   u <- paste0(u, "&end_date=", end_date)
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

# One getDownloadCounts page: its rows, the next page's url (NA on the last)
# and total, the size of the whole collection Launchpad reports on every page
# (NA when the page does not carry it).
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
  total <- suppressWarnings(as.numeric(j$total_size))
  list(rows = rows, next_link = if (is.null(nl)) NA_character_ else as.character(nl),
       total = if (length(total) == 1L) total else NA_real_)
}

# Whether the rows one item's pages returned are all of it, and each once: a
# first page that reported the collection's total (`total`, NA when it did not)
# must be matched by exactly that many rows. A later page that answers empty
# with no next link ends the paging early, which otherwise reads as a complete
# answer. More rows mean the collection grew while it was paged: pages are cut
# by offset over a list sorted newest day first, so a row Launchpad adds before
# the next page shifts it by one and it repeats the row before, which summed
# would overcount that day for good.
all_rows_came <- function(rows, total) is.na(total) || NROW(rows) == total

# Whether a later page's total (NULL or NA when it reports none) agrees with
# the first page's (NA when it reported none). A different total is a
# collection that changed while it was paged, even when the rows add up.
same_total <- function(first, later)
  is.na(first) || length(later) != 1L || is.na(later) || later == first

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

# The roster's done column says how far a release's full history is known:
#   0  never fetched (or its fetch failed): unfetched
#   2  fetched in full once, and Launchpad answered no rows: empty once. A
#      single empty page can be a transient answer, so the monthly update asks
#      for the whole history again before believing it.
#   1  settled: fetched with rows (last_day is its newest day), or answered
#      empty twice (last_day NA: never downloaded)
# A missing or unknown value counts as never fetched.
count_unfetched  <- function(done) sum(is.na(done) | !done %in% c(1L, 2L))
count_empty_once <- function(done) sum(done %in% 2L)

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

# The summary's 30/90/365-day windows and trend end on anchor_date. `edges`
# ({package: YYYY-MM-DD}, a manifest's package_edges) anchors each listed
# package on its own day instead: its data stops there, so windows ending on
# anchor_date would count only the days up to it and make the package look
# as if its downloads had collapsed.
build_summary <- function(daily_con, identity_df, anchor_date, prior_summary = NULL,
                          edges = NULL) {
  a <- format(as.Date(anchor_date), "%Y-%m-%d")
  anchors <- if (length(edges) == 0L)
    data.frame(package = character(0), anchor = character(0), stringsAsFactors = FALSE)
  else data.frame(package = names(edges),
                  anchor = format(as.Date(as.character(unlist(edges, use.names = FALSE)))),
                  stringsAsFactors = FALSE)
  DBI::dbExecute(daily_con, "DROP TABLE IF EXISTS temp.c2d4u_summary_anchor")
  DBI::dbExecute(daily_con,
    "CREATE TEMP TABLE c2d4u_summary_anchor (package TEXT PRIMARY KEY, anchor TEXT NOT NULL)")
  on.exit(DBI::dbExecute(daily_con, "DROP TABLE IF EXISTS temp.c2d4u_summary_anchor"), add = TRUE)
  if (nrow(anchors) > 0L)
    DBI::dbWriteTable(daily_con, "c2d4u_summary_anchor", anchors, append = TRUE)
  agg <- DBI::dbGetQuery(daily_con, sprintf("
    SELECT package,
      MIN(date) AS first_date, MAX(date) AS last_date, SUM(count) AS cnt_total,
      SUM(CASE WHEN date >= date(a,'-30 days')  THEN count ELSE 0 END) AS total_30d,
      SUM(CASE WHEN date >= date(a,'-90 days')  THEN count ELSE 0 END) AS total_90d,
      SUM(CASE WHEN date >= date(a,'-365 days') THEN count ELSE 0 END) AS total_365d,
      SUM(CASE WHEN date >  date(a,'-60 days')
                AND date <  date(a,'-30 days') THEN count ELSE 0 END) AS prev_30d
    FROM (SELECT d.package, d.date, d.count, COALESCE(x.anchor, '%1$s') AS a
          FROM %2$s d LEFT JOIN temp.c2d4u_summary_anchor x ON x.package = d.package)
    GROUP BY package", a, DAILY_TABLE))

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
#'   3. sha256sum (coreutils)  - present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    - macOS/local fallback
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
#'   * db_filename - basename of the file
#'   * db_bytes    - byte size of the file as a double. Deliberately NOT cast
#'                   to integer: R's integer range is 32-bit and overflows to
#'                   NA (serialized as the string "NA") for files >= ~2 GiB.
#'   * db_sha256   - lowercase hex sha256 of the file's exact bytes
#'   * tables      - named list mapping each user table to its row count
#'   * complete    - passed through by the caller. complete = the DB holds the
#'                   full, non-partial dataset (a full rebuild each run);
#'                   freshness is tracked separately via generated_at and the
#'                   run tag. A pipeline with a genuine partial/bootstrap state
#'                   would derive this instead of hardcoding it.
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # Enumerate tables/row counts, then close the connection BEFORE the raw-byte
  # reads below, so no open handle or journal file races the hash/size.
  tables <- tryCatch({
    tbl_names <- DBI::dbGetQuery(con, "
      SELECT name FROM sqlite_master
       WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
       ORDER BY name")$name

    stats::setNames(
      lapply(tbl_names, function(t) {
        DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
      }),
      tbl_names
    )
  }, finally = DBI::dbDisconnect(con))

  # db_bytes/db_sha256 read the raw on-disk file only after the connection
  # above is closed, so no open handle or journal file skews the hash/size.
  list(
    db_filename = basename(db_path),
    db_bytes    = file.size(db_path),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

# Fingerprint of one published db asset, recorded as its manifest shards entry
# so the next run can prove it downloaded exactly what was published: sha256 of
# the file bytes plus rows, sum and date range. A daily shard (year or recent)
# is described by its daily table: sum is SUM(count). The summary DB has no
# daily table and is described by its summary table: rows are packages, sum is
# SUM(cnt_total) and the dates span first_date..last_date. sum is a double so a
# large total never overflows R's 32-bit integer.
asset_fingerprint <- function(path) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  # Read the aggregates, then close the connection BEFORE hashing the bytes.
  agg <- tryCatch({
    tabs <- DBI::dbListTables(con)
    sql <- if (DAILY_TABLE %in% tabs)
      sprintf("SELECT COUNT(*) AS n, SUM(count) AS s, MIN(date) AS lo, MAX(date) AS hi FROM %s",
              DAILY_TABLE)
    else if (SUMMARY_TABLE %in% tabs)
      sprintf("SELECT COUNT(*) AS n, SUM(cnt_total) AS s, MIN(first_date) AS lo,
                      MAX(last_date) AS hi FROM %s", SUMMARY_TABLE)
    else stop("asset_fingerprint: ", basename(path), " has neither a ", DAILY_TABLE,
              " nor a ", SUMMARY_TABLE, " table")
    DBI::dbGetQuery(con, sql)
  }, finally = DBI::dbDisconnect(con))
  list(sha256   = file_sha256(path),
       rows     = as.integer(agg$n),
       sum      = if (is.na(agg$s)) 0 else as.numeric(agg$s),
       date_min = as.character(agg$lo),
       date_max = as.character(agg$hi))
}

# Check that the assets downloaded from the published release into `out_dir`
# are exactly the ones its manifest describes, before anything builds on them.
# Every asset named in prev_manifest$shards must be present with the recorded
# sha256, and no year, recent or summary shard may be present that the manifest
# does not name. A publish uploads the shards one by one and the manifest last,
# so a publish that failed partway leaves newer shards next to the old manifest
# and roster; building on that would count the same downloads twice. Stops
# listing every problem, else returns the verified names invisibly.
# require_fingerprints = FALSE accepts entries without a sha256 (manifests from
# before fingerprints existed) as long as the file is present; it still rejects
# an unnamed year shard, but not an unnamed recent or summary shard, which those
# manifests never listed.
verify_release <- function(out_dir, prev_manifest, require_fingerprints = TRUE) {
  shards <- prev_manifest$shards %||% list()
  named  <- names(shards) %||% character(0)
  problems <- character(0)
  for (f in named) {
    p <- file.path(out_dir, f)
    want <- shards[[f]]$sha256
    if (!file.exists(p)) {
      problems <- c(problems, sprintf("%s is in the manifest but was not downloaded", f))
    } else if (is.null(want)) {
      if (isTRUE(require_fingerprints))
        problems <- c(problems, sprintf("%s has no sha256 in the manifest", f))
    } else if (!identical(file_sha256(p), tolower(as.character(want)))) {
      problems <- c(problems, sprintf("%s does not match its manifest sha256", f))
    }
  }
  local <- list.files(out_dir,
    pattern = sprintf("^%s-(20[0-9]{2}|recent|summary)\\.db$", SHARD_PREFIX))
  extra <- setdiff(local, named)
  if (!isTRUE(require_fingerprints))
    extra <- extra[grepl(sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX), extra)]
  if (length(extra))
    problems <- c(problems, sprintf("%s is not in the manifest", extra))
  if (length(problems))
    stop("torn or foreign release: ", paste(problems, collapse = "; "),
         ". The published assets are not one complete publish of this pipeline;",
         " complete the publish that stopped partway from its workflow artifact (see the",
         " README), or run backfill.yml to republish them", call. = FALSE)
  invisible(named)
}

# The manifest every publisher of a summed release writes (the backfill merge
# and the monthly update), built in one place so the two cannot drift. `base`
# carries the publisher's own fields (tag, timestamps, source_kind,
# changed_shards, summary, and the previous shards map, if any); this adds:
#   history_method     HISTORY_METHOD: the daily history sums every release's
#                      downloads per (package, date). The update refuses a
#                      release without it.
#   counted_through    last day fully counted (YYYY-MM-DD); summary.latest_date
#                      is set to it too, since the summary is anchored on it.
#   package_edges      {package: YYYY-MM-DD} for packages whose data stops
#                      before counted_through (their refresh was held back).
#   known_gaps, detected_gaps (each {from, to}, and kind "stopped" on a quiet
#                      stretch counted as downloads that stopped),
#                      coverage_scope, unfetched_releases
#   empty_once_releases  releases fetched in full once with no rows (done 2),
#                      which the next monthly update asks for again
#   refetch_from, package_refetch_from
#                      the refetch floors: the next monthly update starts every
#                      package's window no later than refetch_from (a day, or
#                      absent) and a listed package's no later than its own
#                      day, so days a run counted beyond the next window's
#                      usual start are fetched a second time
#   shards             each of published_files (in out_dir) fingerprinted by
#                      asset_fingerprint over the base map; other entries kept.
#   integrity core     summary_integrity_core(summary_path) at top level, with
#                      complete = a summed history, no unfetched release, no
#                      release answered empty only once and no held package.
# package_edges, unfetched_releases, empty_once_releases, detected_gaps and
# the refetch floors have no default: a republish (a reclassify) must carry
# forward what the previous manifest says. An empty default would publish a
# release with nothing held back, unfetched or awaiting a second answer, which
# reads as complete, or drop a floor, leaving days fetched only once.
# history_method is the method of the history being published. A publisher
# that built a summed history keeps the default; a republish that did not
# rebuild the history passes the previous manifest's value. NULL (the release
# from before the summed backfill, which kept one release's partial count per
# package-day) publishes neither history_method nor counted_through, so the
# monthly update still refuses it, and complete is FALSE; counted_through then
# only anchors summary.latest_date.
# Call it after every published file is final: the fingerprints and the core
# hash the bytes on disk.
contract_manifest <- function(base, out_dir, published_files, counted_through,
                              package_edges, unfetched_releases, empty_once_releases,
                              detected_gaps, refetch_from, package_refetch_from,
                              history_method = HISTORY_METHOD,
                              summary_path = file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))) {
  unstated <- c("package_edges", "unfetched_releases", "empty_once_releases", "detected_gaps",
                "refetch_from", "package_refetch_from")[
    c(missing(package_edges), missing(unfetched_releases), missing(empty_once_releases),
      missing(detected_gaps), missing(refetch_from), missing(package_refetch_from))]
  if (length(unstated))
    stop("contract_manifest: state ", paste(unstated, collapse = ", "),
         "; a republish carries them forward from the previous manifest")
  if (!is.null(history_method) && !identical(history_method, HISTORY_METHOD))
    stop("contract_manifest: history_method must be \"", HISTORY_METHOD,
         "\" or NULL, not ", deparse(history_method))
  summed <- !is.null(history_method)
  ct <- if (is.null(counted_through) || length(counted_through) != 1L) NA
        else as.Date(as.character(counted_through), format = "%Y-%m-%d")
  if (is.na(ct)) stop("contract_manifest: counted_through must be one YYYY-MM-DD date")
  ct <- format(ct, "%Y-%m-%d")
  count <- function(x, what) {
    n <- suppressWarnings(as.integer(x))
    if (length(n) != 1L || is.na(n) || n < 0L)
      stop("contract_manifest: ", what, " must be a non-negative count")
    n
  }
  unfetched  <- count(unfetched_releases, "unfetched_releases")
  empty_once <- count(empty_once_releases, "empty_once_releases")

  # {package: YYYY-MM-DD}, sorted by package; {} (not []) when empty.
  date_map <- function(x, what) {
    x <- as.list(x)
    if (length(x) == 0L) return(stats::setNames(list(), character(0)))
    if (is.null(names(x)) || any(!nzchar(names(x))))
      stop("contract_manifest: ", what, " must be named by package")
    x <- lapply(x, function(v) format(as.Date(v), "%Y-%m-%d"))
    x[order(names(x))]
  }
  edges  <- date_map(package_edges, "package_edges")
  floors <- date_map(package_refetch_from, "package_refetch_from")
  floor_all <- if (!is.null(refetch_from)) {
    f <- if (length(refetch_from) == 1L) as.Date(as.character(refetch_from), format = "%Y-%m-%d")
    if (length(f) != 1L || is.na(f))
      stop("contract_manifest: refetch_from must be NULL or one YYYY-MM-DD date")
    format(f, "%Y-%m-%d")
  }
  # {from, to}, plus kind "stopped" on a stretch counted as downloads that
  # stopped (the monthly update fetches it again while it ends at
  # counted_through).
  gap <- function(from, to, kind = NULL) {
    g <- list(from = as.character(from), to = as.character(to))
    if (length(kind) && !is.na(kind)) {
      if (!identical(as.character(kind), "stopped"))
        stop("contract_manifest: a detected gap's kind must be \"stopped\", not ", deparse(kind))
      g$kind <- "stopped"
    }
    g
  }
  gaps <- if (is.data.frame(detected_gaps))
    lapply(seq_len(nrow(detected_gaps)), function(i)
      gap(detected_gaps$from[i], detected_gaps$to[i], detected_gaps$kind[i]))
  else lapply(detected_gaps, function(g) gap(g$from, g$to, g$kind))

  fps <- stats::setNames(lapply(file.path(out_dir, published_files), asset_fingerprint),
                         published_files)
  out <- base
  out$shards <- merge_shard_coverage(base$shards, fps)
  out$summary <- base$summary %||% list()
  out$summary$latest_date <- ct
  # Assigning NULL drops a marker the base may carry, so an unsummed history
  # never inherits one.
  out$history_method     <- if (summed) HISTORY_METHOD
  out$counted_through    <- if (summed) ct
  out$package_edges      <- edges
  out$known_gaps         <- KNOWN_SOURCE_GAPS
  out$detected_gaps      <- gaps
  out$coverage_scope     <- COVERAGE_SCOPE
  out$unfetched_releases <- unfetched
  out$empty_once_releases <- empty_once
  out$refetch_from       <- floor_all              # NULL drops a stale one
  out$package_refetch_from <- floors
  core <- summary_integrity_core(summary_path,
                                 complete = summed && unfetched == 0L && empty_once == 0L &&
                                            length(edges) == 0L)
  for (k in names(core)) out[[k]] <- core[[k]]
  out
}

# Name the database assets a publisher rebuilt, for the workflow's publish step
# (UPLOAD_LIST in out_dir). `gh release upload --clobber` deletes an asset
# before uploading its replacement, so re-uploading one that did not change
# only risks losing it; a heartbeat lists none and a reclassify only the
# recent and summary shards.
write_upload_list <- function(out_dir, files) {
  writeLines(as.character(files), file.path(out_dir, UPLOAD_LIST))
  invisible(files)
}

#' Serialize the manifest object to JSON.
#'
#' `core` (optional) is a named list of TOP-LEVEL fields to merge into the
#' manifest - used to attach the integrity/completeness core built by
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

# Fetch every page of one collection serially. Stops when a page fails, when a
# later page reports another total than the first (same_total), and when the
# pages did not return exactly the first page's total (all_rows_came), so a
# caller counts the item as failed rather than short or counted twice.
paginate <- function(fetch, first_url, parse_fn, field) {
  acc <- list(); url <- first_url; guard <- 0L; total <- NA_real_
  while (length(url) == 1L && !is.na(url)) {
    guard <- guard + 1L
    if (guard > 100000L) stop("paginate: runaway paging")
    txt <- fetch(url)
    if (is.null(txt)) stop("paginate: fetch failed for ", url)
    pr <- parse_fn(txt)
    if (guard == 1L) total <- pr$total %||% NA_real_
    else if (!same_total(total, pr$total))
      stop(sprintf("paginate: the collection of %s changed size while it was paged (%s, then %s)",
                   first_url, format(total), format(pr$total)))
    acc[[length(acc) + 1L]] <- pr[[field]]
    url <- pr$next_link
  }
  if (length(acc) == 0L) return(NULL)
  out <- do.call(rbind, acc)
  if (!all_rows_came(out, total))
    stop(sprintf("paginate: the pages of %s returned %d rows, not the %s the first page reported",
                 first_url, NROW(out), format(total)))
  out
}

# ---------------------------------------------------------------------------
# CONCURRENCY POOL. Fetch every url with a bounded curl::multi pool, then make
# several retry passes over the failed/NULL indices with growing sleeps so a 503
# wave is ridden out rather than dropping data. Returns a list aligned to `urls`:
# the response body string on HTTP 200, NULL otherwise.
#
# `deadline` (a POSIXct on the `now` clock, or Inf for none) bounds the whole
# call: each block's multi_run gets only the seconds left, requests still in
# flight when it expires are cancelled, and no further block, backoff sleep or
# retry pass starts. Urls not reached stay NULL, the same as a failed fetch, so
# callers treat them as unfetched.
fetch_pool <- function(urls, pool = POOL, passes = FETCH_PASSES, block = 1500L,
                       deadline = Inf, now = Sys.time) {
  out <- vector("list", length(urls))
  n <- length(urls)
  if (n == 0L) return(out)
  secs_left <- function() as.numeric(deadline) - as.numeric(now())
  run <- function(idxs) {
    for (s in seq(1L, length(idxs), by = block)) {
      left <- secs_left()
      if (left <= 0) return(invisible(NULL))
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
      curl::multi_run(timeout = left, pool = p)
      # multi_run returns at the deadline with requests still queued or in
      # flight; cancel them so their connections close and their urls stay NULL.
      for (h in curl::multi_list(p)) curl::multi_cancel(h)
    }
  }
  run(seq_len(n))
  for (k in seq_len(passes - 1L)) {
    failed <- which(vapply(out, is.null, logical(1)))
    if (length(failed) == 0L) break
    if (secs_left() <= 3 * k) break   # no time left for the backoff and another pass
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
# where the item fully completed with no failed page and, when its first page
# reported the collection's total, with every later page reporting the same
# total and exactly that many rows: see same_total and all_rows_came).
#
# `deadline` (a POSIXct on the `now` clock, or Inf) stops the paging: no wave
# starts once it has passed, and items still paging keep ok = FALSE. It is also
# passed to a fetch_many that has a `deadline` argument (the real pool), so a
# wave in flight stops at it too; a plain function(urls) is called unchanged.
# data for an item that is not ok may hold the pages fetched before it stopped,
# so callers must never count rows from an item that is not ok.
fetch_paginated <- function(fetch_many, first_urls, parse_fn, field,
                            deadline = Inf, now = Sys.time) {
  n <- length(first_urls)
  acc <- vector("list", n)         # accumulated field rows per item
  ok  <- logical(n)                # settled-complete flag per item
  total <- rep(NA_real_, n)        # the collection size the first page reported
  seen <- logical(n)               # the first page has been parsed
  cur <- first_urls                # current url to fetch per item
  active <- seq_len(n)
  guard <- 0L
  many <- if ("deadline" %in% names(formals(fetch_many)))
            function(u) fetch_many(u, deadline = deadline) else fetch_many
  while (length(active) > 0L) {
    guard <- guard + 1L
    if (guard > 100000L) stop("fetch_paginated: runaway paging")
    if (as.numeric(now()) >= as.numeric(deadline)) break
    bodies <- many(cur[active])
    nxt <- integer(0)
    for (m in seq_along(active)) {
      i <- active[m]; body <- bodies[[m]]
      if (is.null(body)) next          # failed page -> item stays ok=FALSE
      pr <- tryCatch(parse_fn(body), error = function(e) NULL)
      if (is.null(pr)) next
      if (!seen[i]) { total[i] <- pr$total %||% NA_real_; seen[i] <- TRUE }
      else if (!same_total(total[i], pr$total)) next   # changed while paged: not ok
      acc[[i]] <- if (is.null(acc[[i]])) pr[[field]] else rbind(acc[[i]], pr[[field]])
      nl <- pr$next_link
      if (length(nl) == 1L && !is.na(nl)) { cur[i] <- nl; nxt <- c(nxt, i) }
      else ok[i] <- all_rows_came(acc[[i]], total[i])
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
# through `fetch_many` (concurrent). Names that 503 or 404 contribute no rows,
# so the names whose query failed are counted in the log (the first ten by
# name): without it a Launchpad outage during the enumerate would shrink the
# roster silently. Returns entries tagged with the archive key
# (empty-with-archive if none).
enumerate_names <- function(fetch_many, candidates, archive, batch = ENUM_BATCH) {
  empty <- cbind(archive = character(0),
    data.frame(pub_id = integer(0), binary_name = character(0),
               version = character(0), arch = character(0),
               status = character(0), date_published = character(0),
               stringsAsFactors = FALSE))
  if (length(candidates) == 0L) return(empty)
  acc <- list(); failed <- character(0)
  for (nm in split(candidates, ceiling(seq_along(candidates) / batch))) {
    urls <- vapply(nm, function(x) lp_name_query_url(archive, x), character(1))
    res <- fetch_paginated(fetch_many, urls, parse_published_page, "entries")
    failed <- c(failed, nm[!res$ok])
    ent <- do.call(rbind, res$data[res$ok])
    if (!is.null(ent) && nrow(ent) > 0L) acc[[length(acc) + 1L]] <- ent
  }
  shown <- if (length(failed) == 0L) ""
           else sprintf(" (%s%s)", paste(utils::head(failed, 10L), collapse = ", "),
                        if (length(failed) > 10L) ", ..." else "")
  message(sprintf("enumerate %s: %d of %d candidate names failed%s",
                  archive$key, length(failed), length(candidates), shown))
  if (length(acc) == 0L) return(empty)
  cbind(archive = archive$key, do.call(rbind, acc), stringsAsFactors = FALSE)
}

# EVEN sharding by row index modulo N (first-letter buckets are very uneven).
# Shard i owns roster rows where ((rownumber - 1) %% N) == i.
shard_rows <- function(n, i, N) {
  if (n == 0L || N <= 0L) return(integer(0))
  which(((seq_len(n) - 1L) %% N) == i)
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
