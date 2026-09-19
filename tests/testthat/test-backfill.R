# Name-list backfill: enumerate queries per candidate binary name, fetch shards
# the roster evenly by row index modulo N, merge folds the partials.
test_that("run_enumerate writes a ledger-resolved roster via per-name queries", {
  out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"),
    states = c(ggplot2 = "archived"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  rel <- load_releases(rp)
  expect_identical(rel$package, "ggplot2")
  expect_identical(rel$pub_id, 10L)
  expect_identical(rel$origin, "cran")
  expect_identical(rel$canonical_name, "ggplot2")
  expect_identical(rel$identity_state, "archived")
})

test_that("run_enumerate keeps every published release, reset to unfetched", {
  # The archive is frozen, so a release never legitimately disappears; a name
  # dropped from the live indexes (or whose query failed) must not take its
  # published history with it.
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2", oldpkg = "OldPkg"), bioc = c(biobase = "Biobase"),
    states = c(oldpkg = "archived"))
  prev <- mk_roster(c("r-cran-ggplot2", "r-cran-oldpkg"), version = c("3.4.4", "0.1"),
                    pub_id = c(10L, 77L), last_day = c("2024-02-01", "2019-03-01"), done = 1L)
  prev$cnt_total <- c(40L, 3L); prev$identity_state <- "live"; prev$canonical_name <- "stale"
  seed_release(pub, prev, data.frame(package = c("ggplot2", "oldpkg"),
                                     date = c("2024-02-01", "2019-03-01"), count = c(40L, 3L)))
  io <- fake_io(pub, pages = mk_pages(), cran = "ggplot2", ledger = led)
  expect_message(rp <- run_enumerate(io, out, live_floor = 1L, bioc_floor = 1L),
                 "1 enumerated, 1 more from the published roster")
  rel <- load_releases(rp)
  rel <- rel[order(rel$pub_id), ]
  expect_identical(rel$pub_id, c(10L, 77L))
  expect_identical(rel$package, c("ggplot2", "oldpkg"))
  expect_identical(rel$version, c("3.4.4", "0.1"))
  expect_identical(rel$done, c(0L, 0L))
  expect_identical(rel$last_day, c(NA_character_, NA_character_))
  expect_identical(rel$cnt_total, c(NA_integer_, NA_integer_))
  # identity comes from the ledger for the published-only release too
  expect_identical(rel$canonical_name, c("ggplot2", "OldPkg"))
  expect_identical(rel$identity_state, c("live", "archived"))
})

test_that("run_enumerate stops when the published roster cannot be downloaded", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  writeLines("{}", file.path(pub, "manifest.json"))   # a release without its recent shard
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"))
  io <- fake_io(pub, pages = mk_pages(), cran = "ggplot2", ledger = led)
  expect_error(run_enumerate(io, out, live_floor = 1L, bioc_floor = 1L), "published roster")
  expect_false(file.exists(file.path(out, ROSTER_FILE)))
})

test_that("run_enumerate logs the candidate names whose query failed", {
  out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(ggplot2 = "ggplot2", zoo = "zoo"),
                       bioc = c(biobase = "Biobase"))
  # every candidate answers except r-cran-zoo, whose query fails in each archive
  pages <- mk_pages()
  empty <- '{"start":0,"total_size":0,"entries":[]}'
  cand <- setdiff(candidate_binary_names(c("ggplot2", "zoo"), character(0)), "r-cran-zoo")
  for (a in ARCHIVES) for (b in cand) {
    u <- lp_name_query_url(a, b)
    if (is.null(pages[[u]])) pages[[u]] <- empty
  }
  io <- fake_io(withr::local_tempdir(), pages = pages, cran = c("ggplot2", "zoo"), ledger = led)
  msgs <- testthat::capture_messages(rp <- run_enumerate(io, out, live_floor = 1L, bioc_floor = 1L))
  n <- length(cand) + 1L
  expect_true(any(grepl(sprintf("c2d4u4.0+: 1 of %d candidate names failed (r-cran-zoo)", n),
                        msgs, fixed = TRUE)))
  expect_true(any(grepl(sprintf("c2d4u3.5: 1 of %d candidate names failed", n), msgs, fixed = TRUE)))
  expect_identical(load_releases(rp)$pub_id, 10L)
})

test_that("run_fetch_shard fetches counts for its even shard and aggregates", {
  out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  sp <- run_fetch_shard(bf_io(mk_pages()), out, rp, i = 0L, N = 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), sp); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT package,date,count FROM c2d4u_downloads_daily")
  expect_identical(got$count, 40L)
  expect_identical(got$date, "2024-02-01")
  # the release lands only in the shard whose index it maps to
  empty <- run_fetch_shard(bf_io(mk_pages()), out, rp, i = 1L, N = 2L)
  con2 <- DBI::dbConnect(RSQLite::SQLite(), empty); on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_identical(nrow(DBI::dbGetQuery(con2, "SELECT * FROM c2d4u_downloads_daily")), 0L)
})

test_that("run_fetch_shard records when it started fetching and which shard it is", {
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-abc"))
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", "2026-07-01", 3)
  pages[[cu(2)]] <- counts_json("r-cran-abc", "1.0", "2026-07-01", 4)
  t0 <- as.POSIXct("2026-07-06 01:00:00", tz = "UTC")
  tick <- 0
  clock <- function() { tick <<- tick + 1; t0 + (tick - 1) * 3600 }   # an hour per reading
  sp <- run_fetch_shard(fake_io(d, pages, now = clock), d, rp, i = 1L, N = 2L)
  meta <- read_part_meta(sp)
  expect_identical(meta$fetched_at, "2026-07-06T01:00:00Z")   # the first reading
  expect_identical(meta$shard_i, 1L)
  expect_identical(meta$shard_n, 2L)
})

test_that("run_fetch_shard leaves the roster cnt_total unset", {
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-zoo"), version = c("1", "2"))
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1", "2026-07-01", 3)
  pages[[cu(2)]] <- counts_json("r-cran-zoo", "2", "2026-07-01", 4)
  part <- read_part(run_fetch_shard(fake_io(d, pages), d, rp, i = 0L, N = 1L))
  expect_identical(part$daily$count, 7L)
  expect_identical(part$roster$done, c(1L, 1L))
  expect_true(all(is.na(part$roster$cnt_total)))
})

test_that("a release that fails in the pool is recovered by the serial retry", {
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-abc"))
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", c("2026-07-02", "2026-07-01"), c(5, 6))
  pages[[cu(2)]] <- counts_json("r-cran-abc", "1.0", "2026-06-30", 4)
  io <- recording_io(d, pages = pages, pool_pages = pages[cu(1)])
  part <- read_part(run_fetch_shard(io, d, rp, i = 0L, N = 1L))
  expect_identical(io$calls$fetch, cu(2))            # only the failed release is retried serially
  expect_identical(part$roster$done, c(1L, 1L))
  expect_identical(part$roster$last_day, c("2026-07-02", "2026-06-30"))
  expect_identical(part$daily$package, c("abc", "zoo", "zoo"))
  expect_identical(part$daily$count, c(4L, 6L, 5L))
})

test_that("the retry discards the pages a failed release got in the pool", {
  # page 1 arrives in the pool and page 2 fails; the serial retry fetches both,
  # so the result must equal one clean fetch, not page 1 counted twice.
  d <- withr::local_tempdir()
  rp <- bf_roster(d, "r-cran-zoo")
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", "2026-07-02", 5, next_link = "zoo-p2",
                                total_size = 2)
  pages[["zoo-p2"]] <- counts_json("r-cran-zoo", "1.0", "2026-07-01", 7, total_size = 2, start = 1)
  torn  <- read_part(run_fetch_shard(fake_io(d, pages, pool_pages = pages[cu(1)]),
                                     file.path(d, "torn"), rp, i = 0L, N = 1L))
  clean <- read_part(run_fetch_shard(fake_io(d, pages), file.path(d, "clean"), rp, i = 0L, N = 1L))
  expect_identical(clean$daily$count, c(7L, 5L))
  expect_identical(torn$daily, clean$daily)
  expect_identical(torn$roster$done, 1L)
  expect_identical(torn$roster$last_day, "2026-07-02")
})

test_that("a release still failing after the retry stays unfetched and contributes nothing", {
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-abc"))
  pages <- list()
  # zoo's page 2 fails everywhere: its page 1 rows must not be kept
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", "2026-07-02", 5, next_link = "zoo-p2",
                                total_size = 2)
  pages[[cu(2)]] <- counts_json("r-cran-abc", "1.0", "2026-07-01", 4)
  expect_message(
    part <- read_part(run_fetch_shard(fake_io(d, pages), d, rp, i = 0L, N = 1L)),
    "assigned 2, ok after pool 1, recovered by retry 0, still failed 1")
  expect_identical(part$roster$done, c(0L, 1L))
  expect_identical(part$roster$last_day, c(NA_character_, "2026-07-01"))
  expect_identical(part$daily$package, "abc")
})

test_that("a release whose later page answers empty stays unfetched and adds nothing", {
  # Launchpad said zoo has two rows; page 2 came back empty with no next link
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-abc"))
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", "2026-07-02", 5, next_link = "zoo-p2",
                                total_size = 2)
  pages[["zoo-p2"]] <- counts_json("r-cran-zoo", "1.0", character(0), integer(0), total_size = 2,
                                   start = 1)
  pages[[cu(2)]] <- counts_json("r-cran-abc", "1.0", "2026-07-01", 4)
  expect_message(
    part <- read_part(run_fetch_shard(fake_io(d, pages), d, rp, i = 0L, N = 1L)),
    "assigned 2, ok after pool 1, recovered by retry 0, still failed 1")
  expect_identical(part$roster$done, c(0L, 1L))
  expect_identical(part$daily$package, "abc")
})

test_that("a release whose history grew while it was paged is fetched again, not counted twice", {
  # Launchpad adds zoo's 07-04 after the first wave: page 2 then starts a row
  # earlier and repeats 07-02, which summed would double that day for good
  d <- withr::local_tempdir()
  rp <- bf_roster(d, "r-cran-zoo")
  before <- list(); after <- list()
  before[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", c("2026-07-03", "2026-07-02"), c(5, 7),
                                 next_link = "zoo-p2", total_size = 3)
  after[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", c("2026-07-04", "2026-07-03"), c(2, 5),
                                next_link = "zoo-p2", total_size = 4)
  after[["zoo-p2"]] <- counts_json("r-cran-zoo", "1.0", c("2026-07-02", "2026-07-01"), c(7, 9),
                                   total_size = 4, start = 2)
  io <- fake_io(d, after)
  waves <- 0L
  io$fetch_many <- function(urls) {
    waves <<- waves + 1L
    lapply(urls, function(u) (if (waves == 1L) before else after)[[u]])
  }
  expect_message(part <- read_part(run_fetch_shard(io, d, rp, i = 0L, N = 1L)),
                 "assigned 1, ok after pool 0, recovered by retry 1, still failed 0")
  expect_identical(part$roster$done, 1L)
  expect_identical(part$daily$date, c("2026-07-01", "2026-07-02", "2026-07-03", "2026-07-04"))
  expect_identical(part$daily$count, c(9L, 7L, 5L, 2L))
})

test_that("the retry starts no release once its time budget is spent", {
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-a", "r-cran-b", "r-cran-c", "r-cran-d"))
  pages <- list()
  for (k in 1:4)
    pages[[cu(k)]] <- counts_json(paste0("r-cran-", letters[k]), "1.0", "2026-07-01", k)
  io <- fake_io(d, pages, pool_pages = pages[cu(1)])   # b, c and d fail in the pool
  # each serial fetch takes 40 minutes of the 60-minute budget's clock
  t0 <- as.POSIXct("2026-07-06 01:00:00", tz = "UTC"); spent <- 0
  serial <- io$fetch
  io$fetch <- function(url) { spent <<- spent + 40 * 60; serial(url) }
  io$now <- function() t0 + spent
  expect_message(
    part <- read_part(run_fetch_shard(io, d, rp, i = 0L, N = 1L, retry_budget_min = 60)),
    "assigned 4, ok after pool 1, recovered by retry 2, still failed 1")
  expect_identical(part$roster$done, c(1L, 1L, 1L, 0L))
  expect_identical(part$daily$package, c("a", "b", "c"))
})

test_that("an empty answer leaves a release empty once, not settled as never downloaded", {
  # A single empty page may be a transient answer; the monthly update asks
  # again before calling the release never downloaded.
  d <- withr::local_tempdir()
  rp <- bf_roster(d, c("r-cran-zoo", "r-cran-abc", "r-cran-new"))
  pages <- list()
  pages[[cu(1)]] <- counts_json("r-cran-zoo", "1.0", "2026-07-01", 3)
  pages[[cu(2)]] <- counts_json("r-cran-abc", "1.0", character(0), integer(0))
  msgs <- testthat::capture_messages(
    part <- read_part(run_fetch_shard(fake_io(d, pages), d, rp, i = 0L, N = 1L)))
  expect_identical(part$roster$done, c(1L, 2L, 0L))          # new failed: unfetched
  expect_identical(part$roster$last_day, c("2026-07-01", NA, NA))
  expect_true(any(grepl("2 releases fetched, 1 of them with no downloads", msgs)))
})
