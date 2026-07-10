mk_daily_con <- function(df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(df)) DBI::dbWriteTable(con, DAILY_TABLE, df, append = TRUE)
  con
}

test_that("build_summary computes windows, totals, ranks, identity, and identity_state", {
  df <- data.frame(package = c("ggplot2","ggplot2","mass"),
                   date = c("2026-04-20","2026-05-01","2026-05-01"),
                   count = c(10L, 5L, 3L), stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities(c("r-cran-ggplot2","r-cran-mass"),
                              mk_maps(c(ggplot2 = "ggplot2", mass = "MASS"),
                                      c(ggplot2 = "live", mass = "archived")))
  s <- build_summary(con, ident, anchor_date = "2026-05-05")
  expect_identical(s$package, c("ggplot2","mass"))       # ordered by rank_30d then package
  expect_identical(s$canonical_name, c("ggplot2","MASS"))
  expect_identical(s$origin, c("cran","cran"))
  expect_identical(s$identity_state, c("live","archived"))
  expect_identical(names(s)[ncol(s)], "identity_state")
  expect_identical(s$cnt_total[s$package == "ggplot2"], 15L)
  expect_identical(s$first_date[s$package == "ggplot2"], "2026-04-20")
  expect_identical(s$total_30d[s$package == "ggplot2"], 15L)
})

test_that("build_summary carries forward inactive prior packages with their identity_state", {
  df <- data.frame(package = "ggplot2", date = "2026-05-01", count = 5L, stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities("r-cran-ggplot2", mk_maps(c(ggplot2 = "ggplot2"), c(ggplot2 = "live")))
  prior <- empty_summary()
  prior[1, ] <- list("oldpkg","oldpkg","cran","OldPkg",0L,0L,0L,
                     9999L,9999L,9999L,0,NA,"2020-01-01","2021-01-01",42L,"archived")
  s <- build_summary(con, ident, "2026-05-05", prior_summary = prior)
  expect_true("oldpkg" %in% s$package)
  expect_identical(s$cnt_total[s$package == "oldpkg"], 42L)
  expect_identical(s$identity_state[s$package == "oldpkg"], "archived")
  expect_identical(s$first_date[s$package == "oldpkg"], "2020-01-01")
})

test_that("build_summary does not double-count the -30 day in the trend prev window", {
  df <- data.frame(package = "ggplot2",
                   date = c("2026-05-16", "2026-05-31"),
                   count = c(50L, 100L), stringsAsFactors = FALSE)
  con <- mk_daily_con(df); on.exit(DBI::dbDisconnect(con))
  ident <- resolve_identities("r-cran-ggplot2", mk_maps(c(ggplot2 = "ggplot2"), c(ggplot2 = "live")))
  s <- build_summary(con, ident, anchor_date = "2026-06-30")
  expect_identical(s$total_30d, 100L)
  expect_identical(s$trend, 100)
})

# An old shard published before identity_state existed has the summary table
# without that column at all (not just NULL values in it); SELECT * over it
# omits the column entirely, so load_summary must backfill it length-safely.
mk_old_summary_shard <- function(path, row = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (
       package       TEXT,
       package_lower TEXT,
       origin        TEXT,
       canonical_name TEXT,
       total_30d     INTEGER,
       total_90d     INTEGER,
       total_365d    INTEGER,
       rank_30d      INTEGER,
       rank_90d      INTEGER,
       rank_365d     INTEGER,
       avg_daily_30d REAL,
       trend         REAL,
       first_date    TEXT,
       last_date     TEXT,
       cnt_total     INTEGER,
       PRIMARY KEY (package))", SUMMARY_TABLE))
  if (!is.null(row)) DBI::dbWriteTable(con, SUMMARY_TABLE, row, append = TRUE)
}

test_that("load_summary migrates an old shard lacking identity_state to NA", {
  p <- withr::local_tempfile(fileext = ".db")
  row <- data.frame(package = "a", package_lower = "a", origin = "cran", canonical_name = "A",
                    total_30d = 1L, total_90d = 1L, total_365d = 1L,
                    rank_30d = 1L, rank_90d = 1L, rank_365d = 1L,
                    avg_daily_30d = 0.03, trend = NA_real_,
                    first_date = "2026-01-01", last_date = "2026-01-01",
                    cnt_total = 1L, stringsAsFactors = FALSE)
  mk_old_summary_shard(p, row)
  s <- load_summary(p)
  expect_identical(nrow(s), 1L)
  expect_identical(names(s), SUMMARY_COLS)
  expect_true(is.na(s$identity_state))
})

test_that("load_summary migrates a 0-row old shard without crashing", {
  p <- withr::local_tempfile(fileext = ".db")
  mk_old_summary_shard(p)
  s <- load_summary(p)
  expect_identical(nrow(s), 0L)
  expect_identical(names(s), SUMMARY_COLS)
  expect_identical(s$identity_state, character(0))
})
