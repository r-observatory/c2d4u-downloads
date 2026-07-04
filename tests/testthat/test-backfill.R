mk_pages <- function() {
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_published_url(a)]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2024-02-01","count":40}]}'
  pages
}
bf_io <- function(pages, cran = "ggplot2") {
  list(release_exists = function() FALSE,
       release_download = function(pattern, dir) 1L,
       fetch = function(url) pages[[url]] %||% NULL,
       cran_names = function() cran, bioc_names = function() character(0),
       now = function() as.POSIXct("2026-07-04 00:00:00", tz = "UTC"))
}

test_that("run_enumerate writes a roster of resolvable releases", {
  out <- withr::local_tempdir()
  rp <- run_enumerate(bf_io(mk_pages()), out)
  rel <- load_releases(rp)
  expect_identical(rel$package, "ggplot2")
  expect_identical(rel$pub_id, 10L)
})

test_that("run_fetch_shard fetches counts for its bucket and aggregates", {
  out <- withr::local_tempdir()
  rp <- run_enumerate(bf_io(mk_pages()), out)
  sp <- run_fetch_shard(bf_io(mk_pages()), out, rp, shard = "g")
  con <- DBI::dbConnect(RSQLite::SQLite(), sp); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT package,date,count FROM c2d4u_downloads_daily")
  expect_identical(got$count, 40L)
  expect_identical(got$date, "2024-02-01")
})

test_that("run_merge folds shard partials into year shards + summary", {
  out <- withr::local_tempdir(); parts <- withr::local_tempdir()
  rp <- run_enumerate(bf_io(mk_pages()), out)
  file.copy(run_fetch_shard(bf_io(mk_pages()), out, rp, "g"), parts)
  res <- run_merge(bf_io(mk_pages()), out, parts)
  expect_true("c2d4u-downloads-2024.db" %in% res$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package,cnt_total,origin FROM c2d4u_downloads_summary")
  expect_identical(s$cnt_total, 40L)
  expect_identical(s$origin, "cran")
})
