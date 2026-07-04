test_that("coverage summarizes rows", {
  r <- data.frame(package="a", date=c("2026-01-01","2026-03-01"), count=c(1L,2L))
  cov <- coverage(r)
  expect_identical(cov$rows, 2L)
  expect_identical(cov$date_min, "2026-01-01")
  expect_identical(cov$date_max, "2026-03-01")
})

test_that("merge_shard_coverage overlays updates on prior", {
  prev <- list(`c2d4u-downloads-2025.db` = list(rows=1L))
  upd  <- list(`c2d4u-downloads-2026.db` = list(rows=2L))
  m <- merge_shard_coverage(prev, upd)
  expect_named(m, c("c2d4u-downloads-2025.db","c2d4u-downloads-2026.db"), ignore.order = TRUE)
})

test_that("write_manifest round-trips through JSON", {
  p <- withr::local_tempfile(fileext = ".json")
  write_manifest(p, list(source_kind = "launchpad", changed_shards = list()))
  got <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_identical(got$source_kind, "launchpad")
})
