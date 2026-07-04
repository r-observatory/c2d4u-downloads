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
  token <- sub("^r-(cran|bioc|other)-", "", bn)

  canonical <- rep(NA_character_, length(bn))
  is_cran <- pref == "cran"
  is_bioc <- pref == "bioc"
  if (any(is_cran)) {
    mapped <- unname(cran_map[token[is_cran]])
    canonical[is_cran] <- ifelse(is.na(mapped), token[is_cran], mapped)
  }
  if (any(is_bioc)) {
    mapped <- if (!is.null(bioc_map)) unname(bioc_map[token[is_bioc]]) else NA_character_
    canonical[is_bioc] <- ifelse(is.na(mapped), token[is_bioc], mapped)
  }
  # origin='other' keeps canonical_name = NA (off the leaderboard).

  df <- data.frame(binary_name = bn, package = token, origin = pref,
                   canonical_name = canonical, stringsAsFactors = FALSE)
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
