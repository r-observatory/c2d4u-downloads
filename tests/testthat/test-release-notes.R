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

test_that("release notes render n/a (not NA) for missing manifest fields", {
  p <- withr::local_tempfile(fileext = ".md")
  manifest <- list(
    last_checked = NA_character_, last_changed = NA_character_,
    source_kind = NA_character_, changed_shards = list(),
    shards = list(`c2d4u-downloads-2026.db` = list(rows = 5L, date_min = NA_character_, date_max = NA_character_)),
    summary = list(packages = NA_integer_, latest_date = NA_character_))
  write_release_notes(p, manifest)
  txt <- paste(readLines(p), collapse = "\n")
  expect_false(grepl("\\| NA \\|", txt))
  expect_match(txt, "n/a")
})
