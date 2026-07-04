mk_daily_con <- function(df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(df)) DBI::dbWriteTable(con, DAILY_TABLE, df, append = TRUE)
  con
}

test_that("build_summary computes windows, totals, ranks, identity", {
  df <- data.frame(package = c("ggplot2","ggplot2","mass"),
                   date = c("2026-04-20","2026-05-01","2026-05-01"),
                   count = c(10L, 5L, 3L), stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities(c("r-cran-ggplot2","r-cran-mass"),
                              build_cran_map(c("ggplot2","MASS")), NULL)
  s <- build_summary(con, ident, anchor_date = "2026-05-05")
  expect_identical(s$package, c("ggplot2","mass"))       # ordered by rank_30d then package
  expect_identical(s$canonical_name, c("ggplot2","MASS"))
  expect_identical(s$origin, c("cran","cran"))
  expect_identical(s$cnt_total[s$package == "ggplot2"], 15L)
  expect_identical(s$first_date[s$package == "ggplot2"], "2026-04-20")
  expect_identical(s$total_30d[s$package == "ggplot2"], 15L)
})

test_that("build_summary carries forward inactive prior packages", {
  df <- data.frame(package = "ggplot2", date = "2026-05-01", count = 5L, stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities("r-cran-ggplot2", build_cran_map("ggplot2"), NULL)
  prior <- empty_summary()
  prior[1, ] <- list("oldpkg","oldpkg","cran","OldPkg",0L,0L,0L,
                     9999L,9999L,9999L,0,NA,"2020-01-01","2021-01-01",42L)
  s <- build_summary(con, ident, "2026-05-05", prior_summary = prior)
  expect_true("oldpkg" %in% s$package)
  expect_identical(s$cnt_total[s$package == "oldpkg"], 42L)
  expect_identical(s$first_date[s$package == "oldpkg"], "2020-01-01")
})

test_that("build_summary does not double-count the -30 day in the trend prev window", {
  df <- data.frame(package = "ggplot2",
                   date = c("2026-05-16", "2026-05-31"),  # anchor-45 (prev) and anchor-30 (boundary)
                   count = c(50L, 100L), stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities("r-cran-ggplot2", build_cran_map("ggplot2"), NULL)
  s <- build_summary(con, ident, anchor_date = "2026-06-30")
  # total_30d includes the boundary day (100); prev window is the 50 only, so
  # trend = (100/50 - 1) * 100 = 100. With the old <= bound prev would be 150.
  expect_identical(s$total_30d, 100L)
  expect_identical(s$trend, 100)
})
