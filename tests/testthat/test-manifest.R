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

test_that("coverage ignores NA dates rather than poisoning the range", {
  r <- data.frame(package = "a", date = c("2026-01-01", NA, "2026-03-01"),
                  count = c(1L, 2L, 3L), stringsAsFactors = FALSE)
  cov <- coverage(r)
  expect_identical(cov$rows, 3L)
  expect_identical(cov$date_min, "2026-01-01")
  expect_identical(cov$date_max, "2026-03-01")
})

test_that("iso formats a POSIXct as a UTC Z timestamp", {
  expect_identical(iso(as.POSIXct("2026-07-04 12:34:56", tz = "UTC")),
                   "2026-07-04T12:34:56Z")
})

# --- integrity / completeness core -----------------------------------------

# Build a tiny, real summary DB on disk (canonical schema via export_summary_shard).
build_summary_db <- function(n = 3L) {
  tmp <- tempfile(fileext = ".db")
  df <- empty_summary()
  df[seq_len(n), ] <- NA
  df$package       <- paste0("pkg", seq_len(n))
  df$package_lower <- df$package
  df$origin        <- rep("cran", n)
  df$canonical_name <- paste0("Pkg", seq_len(n))
  df$total_30d     <- seq_len(n) * 10L
  df$total_90d     <- seq_len(n) * 30L
  df$total_365d    <- seq_len(n) * 100L
  df$rank_30d      <- seq_len(n)
  df$rank_90d      <- seq_len(n)
  df$rank_365d     <- seq_len(n)
  df$avg_daily_30d <- seq_len(n) * 1.5
  df$trend         <- rep(NA_real_, n)
  df$first_date    <- rep("2020-01-01", n)
  df$last_date     <- rep("2026-01-01", n)
  df$cnt_total     <- seq_len(n) * 200L
  df$identity_state <- rep("live", n)
  export_summary_shard(path = tmp, summary_df = df)
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_summary_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  expect_equal(core$db_bytes, as.integer(file.size(db)))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, stats::setNames(list(3L), SUMMARY_TABLE))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  skip_if_not_installed("digest")
  db <- build_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)
  independent <- tolower(digest::digest(file = db, algo = "sha256"))
  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_summary_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(
    path = tmp,
    obj  = list(tag = "v20260714-000000",
                changed_shards = list("c2d4u-downloads-summary.db"),
                summary = list(packages = 1L)),
    core = core
  )

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$packages, 1L)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, as.integer(file.size(db)))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables[[SUMMARY_TABLE]], 4L)
  expect_true(parsed$complete)
})
