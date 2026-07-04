`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

lp_archive_ref <- function(archive) {
  sprintf("%s/~%s/+archive/ubuntu/%s",
          LP_API_BASE, archive$owner, utils::URLencode(archive$ref, reserved = TRUE))
}

lp_published_url <- function(archive, start = 0L, size = PAGE_SIZE, status = NULL) {
  u <- sprintf("%s?ws.op=getPublishedBinaries&ws.size=%d&ws.start=%d",
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

.build_name_map <- function(names) {
  names <- names[!is.na(names) & nzchar(names)]
  names <- names[!duplicated(tolower(names))]
  stats::setNames(names, tolower(names))
}
build_cran_map <- function(cran_names) .build_name_map(cran_names)
build_bioc_map <- function(bioc_names) .build_name_map(bioc_names)

parse_views_packages <- function(views_text) {
  lines <- unlist(strsplit(views_text, "\n", fixed = TRUE))
  hits <- grep("^Package:\\s*", lines, value = TRUE)
  trimws(sub("^Package:\\s*", "", hits))
}

# Prefix-authoritative origin for c2d4u. Non-r-* names are dropped.
resolve_identities <- function(binary_names, cran_map, bioc_map = NULL) {
  bn <- unique(binary_names)
  pref <- rep(NA_character_, length(bn))
  pref[startsWith(bn, "r-cran-")]  <- "cran"
  pref[startsWith(bn, "r-bioc-")]  <- "bioc"
  pref[startsWith(bn, "r-other-")] <- "other"
  keep <- !is.na(pref)
  bn <- bn[keep]; pref <- pref[keep]
  token <- tolower(sub("^r-(cran|bioc|other)-", "", bn))

  canonical <- rep(NA_character_, length(bn))
  is_cran <- pref == "cran"
  is_bioc <- pref == "bioc"
  if (any(is_cran)) {
    mapped <- unname(cran_map[token[is_cran]])
    canonical[is_cran] <- ifelse(is.na(mapped), token[is_cran], mapped)
  }
  if (any(is_bioc)) {
    mapped <- if (!is.null(bioc_map)) unname(bioc_map[token[is_bioc]]) else rep(NA_character_, sum(is_bioc))
    canonical[is_bioc] <- ifelse(is.na(mapped), token[is_bioc], mapped)
  }
  # origin='other' keeps canonical_name = NA (off the leaderboard).

  df <- data.frame(binary_name = bn, package = token, origin = pref,
                   canonical_name = canonical, stringsAsFactors = FALSE)
  # Keep one row per package token, preferring cran > bioc > other. This must
  # preserve first-appearance order (a plain order()+!duplicated() would sort
  # alphabetically and break callers that rely on input order).
  # One origin per token: cran > bioc > other.
  # Keep the highest-rank (lowest value) occurrence of each token in order of first appearance.
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
             cnt_total = integer(0), stringsAsFactors = FALSE)
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
                AND date <= date('%1$s','-30 days') THEN count ELSE 0 END) AS prev_30d
    FROM %2$s GROUP BY package", a, DAILY_TABLE))

  if (nrow(agg) == 0L && is.null(prior_summary)) return(empty_summary())

  agg$package_lower <- tolower(agg$package)
  agg$avg_daily_30d <- round(agg$total_30d / 30, 2)
  agg$trend <- ifelse(!is.na(agg$prev_30d) & agg$prev_30d > 0,
                      round((agg$total_30d / agg$prev_30d - 1) * 100, 2), NA_real_)
  # The roster has one row per (binary,version); collapse to one identity per
  # package token before joining so the aggregate is not fanned out.
  id1 <- identity_df[!duplicated(identity_df$package), c("package","origin","canonical_name")]
  agg <- merge(agg, id1, by = "package", all.x = TRUE)
  agg$origin <- ifelse(is.na(agg$origin), "other", agg$origin)

  for (col in c("total_30d","total_90d","total_365d","cnt_total"))
    agg[[col]] <- as.integer(agg[[col]])

  cur <- agg[c("package","package_lower","origin","canonical_name",
               "total_30d","total_90d","total_365d","avg_daily_30d","trend",
               "first_date","last_date","cnt_total")]

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
        cnt_total = as.integer(gone$cnt_total), stringsAsFactors = FALSE)
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
