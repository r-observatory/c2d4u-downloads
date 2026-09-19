# Test doubles and builders shared by the backfill tests (enumerate, fetch
# and merge).

mk_pages <- function() {
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_name_query_url(a, "r-cran-ggplot2")]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2024-02-01","count":40}]}'
  pages
}
bf_io <- function(pages, cran = "ggplot2", archive = character(0), bioc = character(0),
                  ledger = NULL) {
  list(release_exists = function() FALSE,
       release_download = function(pattern, dir) 1L,
       fetch = function(url) pages[[url]] %||% NULL,
       fetch_many = function(urls) lapply(urls, function(u) pages[[u]] %||% NULL),
       cran_names = function() cran,
       archive_names = function() archive,
       bioc_names = function() bioc,
       identity_dbs = function() { if (is.null(ledger)) stop("no ledger (test)"); ledger },
       now = function() as.POSIXct("2026-07-04 00:00:00", tz = "UTC"))
}

# The getDownloadCounts first-page url of a mk_roster release (first archive).
cu <- function(pub_id) lp_counts_url(ARCHIVES[[1]], pub_id)

# An enumerated roster file of never-fetched releases, as run_enumerate writes it.
bf_roster <- function(dir, binary_name, version = "1.0", pub_id = seq_along(binary_name)) {
  write_roster(file.path(dir, ROSTER_FILE),
               mk_roster(binary_name, version = version, pub_id = pub_id, done = 0L))
}

read_part <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  list(daily  = DBI::dbGetQuery(con, sprintf(
         "SELECT package, date, count FROM %s ORDER BY package, date", DAILY_TABLE)),
       roster = DBI::dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY pub_id", RELEASES_TABLE)))
}

# Per-release getDownloadCounts rows, as run_fetch_shard reads them.
rel_rows <- function(binary_name, version, day, count)
  data.frame(binary_name = binary_name, version = version, day = day,
             count = as.integer(count), stringsAsFactors = FALSE)

# Write parts 0..N-1 of `roster` into `dir` the way run_fetch_shard splits it:
# part i holds the roster rows shard_rows(i, N) and the per-package sums of
# their releases' `rows`. fetched_at is recycled over the parts.
mk_parts <- function(dir, roster, rows, N, fetched_at = "2026-07-06 01:00:00") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  fetched_at <- rep_len(fetched_at, N)
  for (i in seq_len(N) - 1L) {
    mine <- roster[shard_rows(nrow(roster), i, N), , drop = FALSE]
    r <- rows[paste(rows$binary_name, rows$version) %in% paste(mine$binary_name, mine$version), ,
              drop = FALSE]
    write_part(file.path(dir, sprintf("%s-shard-%d.db", SHARD_PREFIX, i)),
               aggregate_counts(r, mine), mine,
               as.POSIXct(fetched_at[i + 1L], tz = "UTC"), i, N)
  }
  dir
}

# Split `roster` into N parts under d/parts, write the enumerated roster the
# parts came from (`enumerated`, by default the roster itself) to d, and merge
# the parts into d/out. A release published in d/pub is what the merge replaces.
# The merge's stdout (its ::warning:: annotations) is dropped unless
# show_output is TRUE: most fixtures end long before the day they are merged
# through, and the suite runs inside the workflows, where a stray annotation
# would mark every run.
bf_merge <- function(d, roster, rows, N, io = fake_io(file.path(d, "pub")),
                     fetched_at = "2026-07-06 01:00:00", enumerated = roster,
                     show_output = FALSE) {
  mk_parts(file.path(d, "parts"), roster, rows, N, fetched_at)
  rp <- write_roster(file.path(d, ROSTER_FILE), enumerated)
  merge <- function() run_merge(io, file.path(d, "out"), file.path(d, "parts"), N = N,
                                roster_path = rp)
  if (isTRUE(show_output)) return(merge())
  utils::capture.output(res <- merge())
  res
}

# Every daily row of the published year shards, sorted.
out_daily <- function(out) {
  f <- list.files(out, pattern = sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX), full.names = TRUE)
  d <- do.call(rbind, c(list(load_daily(tempfile())), lapply(f, load_daily)))
  d <- d[order(d$package, d$date), , drop = FALSE]
  rownames(d) <- NULL
  d
}

out_summary <- function(out) load_summary(file.path(out, sprintf("%s-summary.db", SHARD_PREFIX)))
