test_that("shard_key buckets by first character", {
  expect_identical(shard_key("ggplot2"), "g")
  expect_identical(shard_key("data.table"), "d")
  expect_identical(shard_key("0mq"), "0")
})

test_that("export_shard round-trips daily rows", {
  d <- data.frame(package = c("a","b"), date = c("2026-01-01","2026-01-02"),
                  count = c(3L,4L), stringsAsFactors = FALSE)
  p <- withr::local_tempfile(fileext = ".db")
  export_shard(p, d)
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT package,date,count FROM c2d4u_downloads_daily ORDER BY package")
  expect_identical(got$count, c(3L,4L))
})

test_that("extract_year and extract_recent filter by date", {
  d <- data.frame(package = "a", date = c("2025-06-01","2026-01-01","2026-06-01"),
                  count = c(1L,2L,3L), stringsAsFactors = FALSE)
  p <- withr::local_tempfile(fileext = ".db"); export_shard(p, d)
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  expect_identical(nrow(extract_year(con, 2026L)), 2L)
  expect_identical(nrow(extract_recent(con, "2026-06-01", 200L)), 2L)  # 2026-01-01 and 2026-06-01
})
