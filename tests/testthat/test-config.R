test_that("config exposes the archive registry and column contract", {
  expect_true(is.list(ARCHIVES) && length(ARCHIVES) == 3)
  expect_identical(ARCHIVES[[1]]$key, "c2d4u4.0+")
  expect_true(ARCHIVES[[1]]$enabled)
  expect_identical(ARCHIVES[[2]]$key, "c2d4u3.5")
  expect_identical(ARCHIVES[[3]]$key, "c2d4u")
  expect_identical(SUMMARY_COLS[1:4],
                   c("package", "package_lower", "origin", "canonical_name"))
  expect_true("cnt_total" %in% SUMMARY_COLS)
})

test_that("config exposes the identity ledger assets and the trailing identity_state column", {
  expect_true("identity_state" %in% SUMMARY_COLS)
  expect_identical(SUMMARY_COLS[length(SUMMARY_COLS)], "identity_state")
  expect_identical(SUMMARY_COLS[15:16], c("cnt_total", "identity_state"))  # existing order preserved
  expect_equal(CRAN_ARCHIVE_REPO, "r-observatory/cran-archive")
  expect_equal(BIOC_META_REPO, "r-observatory/bioconductor-metadata")
  expect_equal(CRAN_NAMES_FLOOR, 15000L)
  expect_equal(BIOC_NAMES_FLOOR, 1500L)
})
