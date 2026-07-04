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
