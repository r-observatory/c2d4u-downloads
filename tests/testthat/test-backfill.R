# Name-list backfill: enumerate queries per candidate binary name, fetch shards
# the roster evenly by row index modulo N, merge folds the partials.
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

test_that("run_enumerate writes a ledger-resolved roster via per-name queries", {
  out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"),
    states = c(ggplot2 = "archived"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  rel <- load_releases(rp)
  expect_identical(rel$package, "ggplot2")
  expect_identical(rel$pub_id, 10L)
  expect_identical(rel$origin, "cran")
  expect_identical(rel$canonical_name, "ggplot2")
  expect_identical(rel$identity_state, "archived")
})

test_that("run_fetch_shard fetches counts for its even shard and aggregates", {
  out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  sp <- run_fetch_shard(bf_io(mk_pages()), out, rp, i = 0L, N = 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), sp); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT package,date,count FROM c2d4u_downloads_daily")
  expect_identical(got$count, 40L)
  expect_identical(got$date, "2024-02-01")
  # the release lands only in the shard whose index it maps to
  empty <- run_fetch_shard(bf_io(mk_pages()), out, rp, i = 1L, N = 2L)
  con2 <- DBI::dbConnect(RSQLite::SQLite(), empty); on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_identical(nrow(DBI::dbGetQuery(con2, "SELECT * FROM c2d4u_downloads_daily")), 0L)
})

test_that("run_merge folds shard partials into year shards + summary with identity_state", {
  out <- withr::local_tempdir(); parts <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"), states = c(ggplot2 = "archived"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  file.copy(run_fetch_shard(bf_io(mk_pages()), out, rp, i = 0L, N = 1L), parts)
  res <- run_merge(bf_io(mk_pages()), out, parts)
  expect_true("c2d4u-downloads-2024.db" %in% res$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package,cnt_total,origin,identity_state FROM c2d4u_downloads_summary")
  expect_identical(s$cnt_total, 40L)
  expect_identical(s$origin, "cran")
  expect_identical(s$identity_state, "archived")
})
