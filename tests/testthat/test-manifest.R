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
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, stats::setNames(list(3L), SUMMARY_TABLE))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

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
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables[[SUMMARY_TABLE]], 4L)
  expect_true(parsed$complete)
})

# --- per-asset fingerprints and the release contract --------------------------

mk_daily_shard <- function(dir, name, df) {
  p <- file.path(dir, name); export_shard(p, df); p
}

test_that("asset_fingerprint describes a daily shard by sha256, rows, sum and date range", {
  d <- withr::local_tempdir()
  p <- mk_daily_shard(d, "c2d4u-downloads-2025.db",
    data.frame(package = c("a", "a", "b"), date = c("2025-01-02", "2025-03-01", "2025-02-01"),
               count = c(3L, 4L, 10L), stringsAsFactors = FALSE))
  fp <- asset_fingerprint(p)
  expect_named(fp, c("sha256", "rows", "sum", "date_min", "date_max"))
  expect_identical(fp$sha256, file_sha256(p))
  expect_identical(fp$rows, 3L)
  expect_identical(fp$sum, 17)
  expect_identical(fp$date_min, "2025-01-02")
  expect_identical(fp$date_max, "2025-03-01")
})

test_that("asset_fingerprint of an empty daily shard has zero rows and sum and no date range", {
  d <- withr::local_tempdir()
  p <- mk_daily_shard(d, "c2d4u-downloads-recent.db",
    data.frame(package = character(0), date = character(0), count = integer(0)))
  fp <- asset_fingerprint(p)
  expect_identical(fp$rows, 0L)
  expect_identical(fp$sum, 0)
  expect_true(is.na(fp$date_min) && is.na(fp$date_max))
})

test_that("asset_fingerprint reads the daily table of a recent shard carrying a summary", {
  d <- withr::local_tempdir()
  p <- mk_daily_shard(d, "c2d4u-downloads-recent.db",
    data.frame(package = "a", date = "2026-01-01", count = 5L))
  s <- empty_summary()
  s[1, ] <- list("a","a","cran","A",5L,5L,5L,1L,1L,1L,0.17,NA,"2020-01-01","2026-01-01",999L,"live")
  embed_aux(p, s, .empty_releases())
  fp <- asset_fingerprint(p)
  expect_identical(fp$rows, 1L)
  expect_identical(fp$sum, 5)
})

test_that("asset_fingerprint describes a summary DB by package rows and summed cnt_total", {
  db <- build_summary_db(3L)   # cnt_total 200, 400, 600; dates 2020-01-01..2026-01-01
  on.exit(unlink(db))
  fp <- asset_fingerprint(db)
  expect_identical(fp$rows, 3L)
  expect_identical(fp$sum, 1200)
  expect_identical(fp$date_min, "2020-01-01")
  expect_identical(fp$date_max, "2026-01-01")
  expect_identical(fp$sha256, file_sha256(db))
})

test_that("asset_fingerprint refuses a DB with neither a daily nor a summary table", {
  p <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "CREATE TABLE other (x INTEGER)")
  DBI::dbDisconnect(con)
  expect_error(asset_fingerprint(p), "neither")
})

# A release directory whose manifest fingerprints every db asset in it.
mk_verified_dir <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  mk_daily_shard(d, "c2d4u-downloads-2025.db",
    data.frame(package = "a", date = "2025-06-01", count = 2L))
  mk_daily_shard(d, "c2d4u-downloads-recent.db",
    data.frame(package = "a", date = "2025-06-01", count = 2L))
  file.copy(build_summary_db(1L), file.path(d, "c2d4u-downloads-summary.db"))
  files <- c("c2d4u-downloads-2025.db", "c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db")
  shards <- stats::setNames(lapply(file.path(d, files), asset_fingerprint), files)
  list(dir = d, manifest = list(shards = shards))
}

test_that("verify_release accepts a release whose assets all match their fingerprints", {
  r <- mk_verified_dir()
  expect_silent(got <- verify_release(r$dir, r$manifest))
  expect_setequal(got, names(r$manifest$shards))
})

test_that("verify_release stops on an asset that differs from its fingerprint", {
  r <- mk_verified_dir()
  # a torn publish: the year shard was replaced but the manifest was not
  mk_daily_shard(r$dir, "c2d4u-downloads-2025.db",
    data.frame(package = "a", date = "2025-06-01", count = 4L))
  expect_error(verify_release(r$dir, r$manifest),
               "torn or foreign release: c2d4u-downloads-2025.db .*run backfill.yml")
})

test_that("verify_release stops when an asset named in the manifest was not downloaded", {
  r <- mk_verified_dir()
  unlink(file.path(r$dir, "c2d4u-downloads-2025.db"))
  expect_error(verify_release(r$dir, r$manifest),
               "torn or foreign release: c2d4u-downloads-2025.db.*not downloaded")
})

test_that("verify_release stops on a release asset the manifest does not name", {
  r <- mk_verified_dir()
  mk_daily_shard(r$dir, "c2d4u-downloads-2011.db",
    data.frame(package = "a", date = "2011-06-01", count = 1L))
  expect_error(verify_release(r$dir, r$manifest),
               "torn or foreign release: c2d4u-downloads-2011.db is not in the manifest")
})

test_that("verify_release requires a fingerprint unless legacy entries are allowed", {
  r <- mk_verified_dir()
  legacy <- r$manifest
  legacy$shards[["c2d4u-downloads-2025.db"]]$sha256 <- NULL
  legacy$shards[["c2d4u-downloads-summary.db"]] <- NULL   # legacy manifests never named it
  expect_error(verify_release(r$dir, legacy), "c2d4u-downloads-2025.db has no sha256")
  expect_silent(verify_release(r$dir, legacy, require_fingerprints = FALSE))
  # a legacy check still needs every named file and still rejects a stray year shard
  mk_daily_shard(r$dir, "c2d4u-downloads-2011.db",
    data.frame(package = "a", date = "2011-06-01", count = 1L))
  expect_error(verify_release(r$dir, legacy, require_fingerprints = FALSE), "2011.db is not in the manifest")
  unlink(file.path(r$dir, c("c2d4u-downloads-2011.db", "c2d4u-downloads-recent.db")))
  expect_error(verify_release(r$dir, legacy, require_fingerprints = FALSE), "recent.db .*not downloaded")
})

test_that("verify_release reports every problem at once", {
  r <- mk_verified_dir()
  unlink(file.path(r$dir, "c2d4u-downloads-recent.db"))
  mk_daily_shard(r$dir, "c2d4u-downloads-2025.db",
    data.frame(package = "a", date = "2025-06-01", count = 9L))
  err <- tryCatch(verify_release(r$dir, r$manifest), error = conditionMessage)
  expect_match(err, "c2d4u-downloads-2025.db")
  expect_match(err, "c2d4u-downloads-recent.db")
})

# A published-looking out dir: two year shards, recent and summary.
mk_publish_dir <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  mk_daily_shard(d, "c2d4u-downloads-2025.db",
    data.frame(package = "a", date = c("2025-06-01", "2025-06-02"), count = c(2L, 3L)))
  mk_daily_shard(d, "c2d4u-downloads-2026.db",
    data.frame(package = "a", date = "2026-09-01", count = 7L))
  mk_daily_shard(d, "c2d4u-downloads-recent.db",
    data.frame(package = "a", date = c("2025-06-01", "2025-06-02", "2026-09-01"), count = c(2L, 3L, 7L)))
  file.copy(build_summary_db(1L), file.path(d, "c2d4u-downloads-summary.db"))
  d
}

contract_base <- function() list(
  tag = "v20260918-010000", generated_at = "2026-09-18T01:00:00Z",
  last_checked = "2026-09-18T01:00:00Z", last_changed = "2026-09-18T01:00:00Z",
  source_kind = "launchpad", archives = list("c2d4u4.0+"),
  changed_shards = list("c2d4u-downloads-2026.db", "c2d4u-downloads-recent.db",
                        "c2d4u-downloads-summary.db"),
  shards = list(`c2d4u-downloads-2025.db` = list(rows = 2L, date_min = "2025-06-01",
                                                 date_max = "2025-06-02", sha256 = "prev")),
  summary = list(packages = 1L, latest_date = "2026-09-01", releases = 1L))

test_that("contract_manifest adds the summed-history contract and fingerprints the published files", {
  d <- mk_publish_dir()
  pub <- c("c2d4u-downloads-2026.db", "c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db")
  m <- contract_manifest(contract_base(), d, pub, counted_through = as.Date("2026-09-16"),
                         package_edges = list(), unfetched_releases = 0L, empty_once_releases = 0L,
                         detected_gaps = list(),
                         refetch_from = NULL, package_refetch_from = list())
  # base fields survive untouched
  expect_identical(m$tag, "v20260918-010000")
  expect_identical(m$last_checked, "2026-09-18T01:00:00Z")
  expect_identical(m$changed_shards, contract_base()$changed_shards)
  expect_identical(m$summary$packages, 1L)
  # the contract
  expect_identical(m$history_method, "summed")
  expect_identical(m$counted_through, "2026-09-16")
  expect_identical(m$summary$latest_date, "2026-09-16")
  expect_length(m$package_edges, 0L)
  expect_identical(m$known_gaps, KNOWN_SOURCE_GAPS)
  expect_identical(m$detected_gaps, list())
  expect_identical(m$coverage_scope, COVERAGE_SCOPE)
  expect_identical(m$unfetched_releases, 0L)
  expect_identical(m$empty_once_releases, 0L)
  # published files are fingerprinted; an unpublished prior entry is kept as is
  for (f in pub) expect_identical(m$shards[[f]], asset_fingerprint(file.path(d, f)))
  expect_identical(m$shards[["c2d4u-downloads-2025.db"]]$sha256, "prev")
  # the integrity core describes the summary DB
  expect_identical(m$db_filename, "c2d4u-downloads-summary.db")
  expect_identical(m$db_sha256, file_sha256(file.path(d, "c2d4u-downloads-summary.db")))
  expect_true(m$complete)
})

test_that("contract_manifest marks the release incomplete while releases are unfetched or packages held", {
  d <- mk_publish_dir()
  pub <- "c2d4u-downloads-summary.db"
  m1 <- contract_manifest(contract_base(), d, pub, "2026-09-16", package_edges = list(),
                          unfetched_releases = 3L, empty_once_releases = 0L, detected_gaps = list(),
                          refetch_from = NULL, package_refetch_from = list())
  expect_false(m1$complete)
  expect_identical(m1$unfetched_releases, 3L)
  m2 <- contract_manifest(contract_base(), d, pub, "2026-09-16",
                          package_edges = list(zoo = "2026-08-02", abc = as.Date("2026-07-30")),
                          unfetched_releases = 0L, empty_once_releases = 0L, detected_gaps = list(),
                          refetch_from = NULL, package_refetch_from = list())
  expect_false(m2$complete)
  expect_identical(m2$package_edges, list(abc = "2026-07-30", zoo = "2026-08-02"))
  # a release that answered one empty page is not settled yet either
  m3 <- contract_manifest(contract_base(), d, pub, "2026-09-16", package_edges = list(),
                          unfetched_releases = 0L, empty_once_releases = 4L, detected_gaps = list(),
                          refetch_from = NULL, package_refetch_from = list())
  expect_false(m3$complete)
  expect_identical(m3$unfetched_releases, 0L)
  expect_identical(m3$empty_once_releases, 4L)
})

test_that("contract_manifest round-trips through JSON with object edges and array gaps", {
  d <- mk_publish_dir()
  p <- file.path(d, "manifest.json")
  gaps <- list(list(from = "2026-08-10", to = "2026-08-14"),
               list(from = "2026-08-20", to = "2026-09-16", kind = "stopped"))
  m <- contract_manifest(contract_base(), d, "c2d4u-downloads-summary.db", "2026-09-16",
                         package_edges = list(), unfetched_releases = 0L, empty_once_releases = 0L,
                         detected_gaps = gaps,
                         refetch_from = NULL, package_refetch_from = list())
  write_manifest(p, m)
  txt <- paste(readLines(p), collapse = "\n")
  expect_match(txt, '"package_edges": \\{\\}')
  back <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_identical(back$history_method, "summed")
  expect_identical(back$counted_through, "2026-09-16")
  expect_identical(back$detected_gaps, gaps)
  expect_identical(back$known_gaps[[1]]$from, "2026-05-06")
  expect_identical(back$shards[["c2d4u-downloads-summary.db"]]$sha256,
                   file_sha256(file.path(d, "c2d4u-downloads-summary.db")))
  expect_true(back$complete)
  edged <- contract_manifest(contract_base(), d, "c2d4u-downloads-summary.db", "2026-09-16",
                             package_edges = c(zoo = "2026-08-02"), unfetched_releases = 0L,
                             empty_once_releases = 0L, detected_gaps = list(),
                             refetch_from = NULL, package_refetch_from = list())
  write_manifest(p, edged)
  expect_identical(jsonlite::fromJSON(p, simplifyVector = FALSE)$package_edges,
                   list(zoo = "2026-08-02"))
})

test_that("contract_manifest overrides stale contract fields carried in the base", {
  d <- mk_publish_dir()
  base <- contract_base()
  base$counted_through <- "2026-08-01"; base$complete <- FALSE
  base$package_edges <- list(zoo = "2026-07-01"); base$unfetched_releases <- 9L
  m <- contract_manifest(base, d, "c2d4u-downloads-summary.db", "2026-09-16",
                         package_edges = list(), unfetched_releases = 0L, empty_once_releases = 0L,
                         detected_gaps = list(),
                         refetch_from = NULL, package_refetch_from = list())
  expect_identical(m$counted_through, "2026-09-16")
  expect_length(m$package_edges, 0L)
  expect_identical(m$unfetched_releases, 0L)
  expect_true(m$complete)
  expect_identical(sum(names(m) == "complete"), 1L)
})

test_that("contract_manifest refuses a missing or malformed counted_through", {
  d <- mk_publish_dir()
  cm <- function(ct, unfetched = 0L, empty_once = 0L)
    contract_manifest(contract_base(), d, character(0), ct, package_edges = list(),
                      unfetched_releases = unfetched, empty_once_releases = empty_once,
                      detected_gaps = list(),
                      refetch_from = NULL, package_refetch_from = list())
  expect_error(cm(NULL), "counted_through")
  expect_error(cm("not-a-date"), "counted_through")
  expect_error(cm("2026-09-16", unfetched = -1L), "unfetched_releases")
  expect_error(cm("2026-09-16", empty_once = NA), "empty_once_releases")
})

test_that("contract_manifest makes every caller state the state a republish carries forward", {
  # Defaulting these would let a republish that forgot them publish a release
  # with no held package and nothing unfetched or awaiting a second answer,
  # which reads as complete.
  d <- mk_publish_dir()
  pub <- "c2d4u-downloads-summary.db"
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 unfetched_releases = 0L, empty_once_releases = 0L,
                                 detected_gaps = list(),
                                 refetch_from = NULL, package_refetch_from = list()),
               "contract_manifest: .*package_edges")
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 package_edges = list(), empty_once_releases = 0L,
                                 detected_gaps = list(),
                                 refetch_from = NULL, package_refetch_from = list()),
               "contract_manifest: .*unfetched_releases")
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 package_edges = list(), unfetched_releases = 0L,
                                 detected_gaps = list(),
                                 refetch_from = NULL, package_refetch_from = list()),
               "contract_manifest: .*empty_once_releases")
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 package_edges = list(), unfetched_releases = 0L,
                                 empty_once_releases = 0L,
                                 refetch_from = NULL, package_refetch_from = list()),
               "contract_manifest: .*detected_gaps")
  # a refetch floor dropped by a republish would leave days fetched only once
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 package_edges = list(), unfetched_releases = 0L,
                                 empty_once_releases = 0L, detected_gaps = list(),
                                 package_refetch_from = list()),
               "contract_manifest: .*refetch_from")
  expect_error(contract_manifest(contract_base(), d, pub, "2026-09-16",
                                 package_edges = list(), unfetched_releases = 0L,
                                 empty_once_releases = 0L, detected_gaps = list(),
                                 refetch_from = NULL),
               "contract_manifest: .*package_refetch_from")
})

test_that("contract_manifest records the refetch floors and drops stale ones", {
  d <- mk_publish_dir()
  p <- file.path(d, "manifest.json")
  cm <- function(base = contract_base(), refetch_from = NULL, package_refetch_from = list())
    contract_manifest(base, d, "c2d4u-downloads-summary.db", "2026-09-16",
                      package_edges = list(), unfetched_releases = 0L, empty_once_releases = 0L,
                      detected_gaps = list(), refetch_from = refetch_from,
                      package_refetch_from = package_refetch_from)
  m <- cm(refetch_from = as.Date("2026-08-02"),
          package_refetch_from = list(zoo = "2026-07-01", abc = as.Date("2026-06-15")))
  expect_identical(m$refetch_from, "2026-08-02")
  expect_identical(m$package_refetch_from, list(abc = "2026-06-15", zoo = "2026-07-01"))
  expect_true(m$complete)                                  # a floor is not a hole
  write_manifest(p, m)
  back <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_identical(back$refetch_from, "2026-08-02")
  expect_identical(back$package_refetch_from, list(abc = "2026-06-15", zoo = "2026-07-01"))
  # none: no global floor, and an empty object
  base <- contract_base()
  base$refetch_from <- "2026-01-01"; base$package_refetch_from <- list(zoo = "2026-01-01")
  none <- cm(base)
  expect_false("refetch_from" %in% names(none))
  expect_length(none$package_refetch_from, 0L)
  write_manifest(p, none)
  expect_match(paste(readLines(p), collapse = "\n"), '"package_refetch_from": \\{\\}')
  expect_error(cm(refetch_from = "soon"), "contract_manifest: refetch_from")
  expect_error(cm(package_refetch_from = list("2026-07-01")), "package_refetch_from")
})

test_that("contract_manifest leaves a history not stated as summed unmarked and incomplete", {
  d <- mk_publish_dir()
  pub <- c("c2d4u-downloads-2025.db", "c2d4u-downloads-2026.db",
           "c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db")
  # a stale marker in the base must not survive either
  base <- contract_base()
  base$history_method <- "summed"; base$counted_through <- "2026-08-01"
  m <- contract_manifest(base, d, pub, "2026-09-01", package_edges = list(),
                         unfetched_releases = 0L, empty_once_releases = 0L, detected_gaps = list(),
                         refetch_from = NULL, package_refetch_from = list(),
                         history_method = NULL)
  expect_false("history_method" %in% names(m))
  expect_false("counted_through" %in% names(m))
  expect_false(m$complete)
  expect_identical(m$summary$latest_date, "2026-09-01")
  for (f in pub) expect_identical(m$shards[[f]], asset_fingerprint(file.path(d, f)))
  p <- file.path(d, "manifest.json")
  write_manifest(p, m)
  back <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_null(back$history_method)
  expect_null(back$counted_through)
  expect_false(back$complete)
})

test_that("contract_manifest refuses a history_method other than summed", {
  d <- mk_publish_dir()
  expect_error(contract_manifest(contract_base(), d, "c2d4u-downloads-summary.db", "2026-09-16",
                                 package_edges = list(), unfetched_releases = 0L,
                                 empty_once_releases = 0L, detected_gaps = list(),
                                 refetch_from = NULL, package_refetch_from = list(),
                                 history_method = "first-wins"),
               "contract_manifest: history_method")
})

test_that("republishing a legacy release through contract_manifest never passes it off as summed", {
  # The release published before the summed backfill: its manifest has no
  # contract fields and its shard entries carry coverage only.
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo"), version = c("1", "2"),
                      last_day = c("2025-12-31", "2026-05-05"))
  seed_release(pub, roster, data.frame(package = "zoo", date = c("2025-12-31", "2026-05-05"),
                                       count = c(4L, 6L)))
  m <- jsonlite::fromJSON(file.path(pub, "manifest.json"), simplifyVector = FALSE)
  legacy <- m[c("tag", "generated_at", "last_checked", "last_changed", "source_kind",
                "archives", "changed_shards", "shards", "summary")]
  legacy$shards <- lapply(legacy$shards[names(legacy$shards) != "c2d4u-downloads-summary.db"],
                          function(s) s[c("rows", "date_min", "date_max")])
  write_manifest(file.path(pub, "manifest.json"), legacy)
  io <- fake_io(pub)
  io$release_download("manifest.json", out)
  io$release_download("c2d4u-downloads-*.db", out)
  prev <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_silent(verify_release(out, prev, require_fingerprints = FALSE))

  # a reclassify republishes it, carrying forward what the previous manifest says
  files <- setdiff(list.files(out), "manifest.json")
  re <- contract_manifest(prev, out, files, prev$summary$latest_date,
                          package_edges = prev$package_edges %||% list(),
                          unfetched_releases = 0L, empty_once_releases = 0L,
                          detected_gaps = prev$detected_gaps %||% list(),
                          refetch_from = prev$refetch_from,
                          package_refetch_from = prev$package_refetch_from %||% list(),
                          history_method = prev$history_method)
  write_manifest(file.path(out, "manifest.json"), re)
  back <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  # its assets are genuine, so the strict check passes; the marker is what the
  # monthly update refuses it on, and it is not complete
  expect_silent(verify_release(out, back))
  expect_null(back$history_method)
  expect_null(back$counted_through)
  expect_identical(back$summary$latest_date, "2026-05-05")
  expect_false(back$complete)
})

test_that("verify_release names both ways to repair a torn release", {
  d <- withr::local_tempdir()
  export_shard(file.path(d, "c2d4u-downloads-2026.db"),
               data.frame(package = "zoo", date = "2026-08-15", count = 1L))
  m <- list(shards = list(`c2d4u-downloads-2026.db` = list(sha256 = strrep("0", 64))))
  err <- expect_error(verify_release(d, m), "torn or foreign release")
  expect_match(conditionMessage(err), "artifact")
  expect_match(conditionMessage(err), "backfill.yml")
})
