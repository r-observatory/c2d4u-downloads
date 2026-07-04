test_that("config exposes the archive registry and column contract", {
  expect_true(is.list(ARCHIVES) && length(ARCHIVES) == 3)
  expect_identical(ARCHIVES[[1]]$key, "c2d4u4.0+")
  expect_true(ARCHIVES[[1]]$enabled)
  expect_false(ARCHIVES[[2]]$enabled)
  expect_identical(SUMMARY_COLS[1:4],
                   c("package", "package_lower", "origin", "canonical_name"))
  expect_true("cnt_total" %in% SUMMARY_COLS)
})
