test_that("aggregate_counts sums to package/date via identity", {
  counts <- data.frame(
    binary_name = c("r-cran-ggplot2","r-cran-ggplot2","r-cran-ggplot2","gsl-bin"),
    version = c("v2204","v2004","v2204","x"),
    day = c("2026-05-01","2026-05-01","2026-05-02","2026-05-01"),
    count = c(3L, 4L, 5L, 9L), stringsAsFactors = FALSE)
  ident <- resolve_identities(counts$binary_name, build_cran_map("ggplot2"), NULL)
  agg <- aggregate_counts(counts, ident)
  expect_identical(agg$package, c("ggplot2", "ggplot2"))
  expect_identical(agg$date, c("2026-05-01", "2026-05-02"))
  expect_identical(agg$count, c(7L, 5L))  # gsl-bin dropped
})

test_that("merge_daily upserts new rows over old and preserves untouched history", {
  old <- data.frame(package = c("a","a","b"), date = c("2026-01-01","2026-03-01","2026-03-01"),
                    count = c(1L,3L,5L), stringsAsFactors = FALSE)
  new <- data.frame(package = "a", date = "2026-03-01", count = 30L, stringsAsFactors = FALSE)
  m <- merge_daily(old, new)
  expect_identical(nrow(m), 3L)
  expect_identical(m$count[m$package == "a" & m$date == "2026-03-01"], 30L)  # new wins
  expect_identical(m$count[m$package == "b"], 5L)                             # dead release preserved
  expect_identical(m$count[m$package == "a" & m$date == "2026-01-01"], 1L)
})
