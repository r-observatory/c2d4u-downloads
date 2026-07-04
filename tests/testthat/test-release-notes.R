test_that("release notes render without em dashes and list shards", {
  p <- withr::local_tempfile(fileext = ".md")
  manifest <- list(
    last_checked = "2026-07-04T00:00:00Z", last_changed = "2026-07-04T00:00:00Z",
    source_kind = "launchpad", changed_shards = list("c2d4u-downloads-2026.db"),
    shards = list(`c2d4u-downloads-2026.db` = list(rows = 10L, date_min = "2026-01-01", date_max = "2026-06-01")),
    summary = list(packages = 3L, latest_date = "2026-05-04"))
  write_release_notes(p, manifest)
  txt <- paste(readLines(p), collapse = "\n")
  expect_false(grepl("—", txt))          # no em dash
  expect_match(txt, "c2d4u-downloads-2026.db")
  expect_match(txt, "gh release download current")
})
