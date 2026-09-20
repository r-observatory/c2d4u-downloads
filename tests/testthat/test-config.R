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
  expect_false(exists("LOAD_BIOC_MAP"))  # live-source config removed
})

test_that("config exposes the update and backfill tuning constants", {
  expect_identical(ACTIVE_WINDOW_DAYS, 365L)
  expect_identical(REVISION_WINDOW_DAYS, 30L)
  expect_identical(SETTLED_LAG_DAYS, 2L)
  expect_identical(UPDATE_BATCH, 1500L)
  expect_identical(DEADLINE_MIN, 280L)
  expect_identical(ZERO_RUN_DAYS, 3L)
  expect_identical(REVISION_DROP_TOL, 0.02)
  expect_identical(REGRESSION_MIN_STORED, 50L)
  expect_identical(REGRESSION_MAX_PACKAGES, 10L)
  expect_identical(W_FAIL_MAX_FRAC, 0.02)
  expect_identical(UNFETCHED_WARN_FRAC, 0.01)
  expect_identical(UNFETCHED_MAX_FRAC, 0.10)
  expect_identical(DOMINANCE_MAX_FRAC, 0.01)
  expect_identical(HISTORY_METHOD, "summed")
  expect_identical(COVERAGE_SCOPE, "amd64 publications only (one per archive, binary, version)")
})

test_that("an update batch is exactly one fetch_pool block", {
  expect_identical(UPDATE_BATCH, eval(formals(fetch_pool)$block))
})

test_that("the recent shard spans the active window plus the revision overlap", {
  expect_gt(RECENT_WINDOW_DAYS, ACTIVE_WINDOW_DAYS + REVISION_WINDOW_DAYS)
})

test_that("KNOWN_SOURCE_GAPS records the 2026 Launchpad hole as inclusive dates", {
  expect_true(is.list(KNOWN_SOURCE_GAPS) && length(KNOWN_SOURCE_GAPS) >= 1L)
  g <- KNOWN_SOURCE_GAPS[[1]]
  expect_identical(g$from, "2026-05-06")
  expect_identical(g$to, "2026-07-11")
  expect_identical(g$note, "Launchpad recorded no downloads")
  for (x in KNOWN_SOURCE_GAPS) {
    expect_false(is.na(as.Date(x$from, "%Y-%m-%d")))
    expect_false(is.na(as.Date(x$to, "%Y-%m-%d")))
    expect_lte(as.numeric(as.Date(x$from)), as.numeric(as.Date(x$to)))
  }
})

test_that("config.R alone holds the entrypoints' tunables and env var names", {
  e <- new.env()
  sys.source(file.path(.c2_root, "scripts", "config.R"), envir = e)
  expect_identical(e$RESIDUAL_MIN_LEFT_MIN, 10)
  expect_identical(e$WINDOW_RETRY_BUDGET_MIN, 60)
  expect_identical(e$CHANGED_SHARDS_CARRY_DAYS, 7)
  expect_identical(e$STALL_MAX_DAYS, 90L)
  expect_identical(e$DORMANT_PROBE_N, 50L)
  expect_identical(e$RETRY_BUDGET_MIN, 150)
  expect_identical(e$DEADLINE_MIN_ENV, "C2D4U_DEADLINE_MIN")
  expect_identical(e$RECLASSIFY_ONLY_ENV, "C2D4U_RECLASSIFY_ONLY")
  expect_identical(e$FORCE_REBUILD_ENV, "C2D4U_FORCE_REBUILD")
})
