# The shared test doubles: fake_io (with the pooled fetch_many), its recording
# variant, and seed_release, which must write a release the monthly update
# accepts as a complete summed publish.

test_that("fake_io serves pages to both fetch and fetch_many, pooled pages overriding", {
  pages <- list(u1 = "one", u2 = "two")
  io <- fake_io(withr::local_tempdir(), pages = pages)
  expect_identical(io$fetch("u1"), "one")
  expect_null(io$fetch("missing"))
  expect_identical(io$fetch_many(c("u2", "missing", "u1")), list("two", NULL, "one"))
  # a url can fail in the pool yet succeed for the serial fetch
  io2 <- fake_io(withr::local_tempdir(), pages = pages, pool_pages = list(u1 = "one"))
  expect_identical(io2$fetch_many(c("u1", "u2")), list("one", NULL))
  expect_identical(io2$fetch("u2"), "two")
})

test_that("fake_io takes a fixed time or a clock function", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  expect_identical(fake_io(withr::local_tempdir(), now = t0)$now(), t0)
  tick <- 0
  io <- fake_io(withr::local_tempdir(), now = function() { tick <<- tick + 60; t0 + tick })
  expect_identical(io$now(), t0 + 60)
  expect_identical(io$now(), t0 + 120)
})

test_that("recording_io logs every serial url, pooled batch and deadline", {
  io <- recording_io(withr::local_tempdir(), pages = list(u1 = "one", u2 = "two"))
  io$fetch("u1")
  io$fetch_many(c("u1", "u2"))
  dl <- as.POSIXct("2026-10-03 10:40:00", tz = "UTC")
  res <- fetch_paginated(io$fetch_many, "u2", function(txt) list(rows = txt, next_link = NA),
                         "rows", deadline = dl, now = function() dl - 60)
  expect_true(res$ok)
  expect_identical(io$calls$fetch, "u1")
  expect_identical(io$calls$many, list(c("u1", "u2"), "u2"))
  expect_identical(io$calls$deadlines, list(Inf, dl))
})

test_that("counts_json builds a getDownloadCounts page parse_counts_page reads back", {
  p <- parse_counts_page(counts_json("r-cran-a", "1.0", c("2026-09-02", "2026-09-01"), c(3, 4),
                                     next_link = "u2"))
  expect_identical(p$rows$day, c("2026-09-02", "2026-09-01"))
  expect_identical(p$rows$count, c(3L, 4L))
  expect_identical(p$next_link, "u2")
  empty <- parse_counts_page(counts_json("r-cran-a", "1.0", character(0), integer(0)))
  expect_identical(nrow(empty$rows), 0L)
  expect_true(is.na(empty$next_link))
})

test_that("mk_roster fills the roster columns with test defaults", {
  r <- mk_roster(c("r-cran-zoo", "r-bioc-limma"), last_day = c("2026-09-01", NA), done = c(1L, 0L))
  expect_identical(names(r), names(.empty_releases()))
  expect_identical(r$package, c("zoo", "limma"))
  expect_identical(r$origin, c("cran", "bioc"))
  expect_identical(r$pub_id, 1:2)
  expect_identical(r$done, c(1L, 0L))
  expect_identical(r$last_day, c("2026-09-01", NA))
})

seed_fixture <- function() {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo", "r-cran-abc"), version = c("1", "2", "1"),
                      last_day = c("2025-12-31", "2026-09-10", "2024-05-01"), done = 1L)
  daily <- data.frame(package = c("zoo", "zoo", "zoo", "abc"),
                      date = c("2024-01-05", "2025-12-31", "2026-09-10", "2024-05-01"),
                      count = c(4L, 6L, 9L, 2L), stringsAsFactors = FALSE)
  list(roster = roster, daily = daily)
}

test_that("seed_release writes a summed release that passes verify_release", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  fx <- seed_fixture()
  m <- seed_release(pub, fx$roster, fx$daily, list(counted_through = "2026-09-14"))
  expect_setequal(list.files(pub), c("manifest.json", "c2d4u-downloads-2024.db",
    "c2d4u-downloads-2025.db", "c2d4u-downloads-2026.db", "c2d4u-downloads-recent.db",
    "c2d4u-downloads-summary.db"))
  # download it the way run_update does and check it against its own manifest
  io <- fake_io(pub)
  expect_true(io$release_exists())
  expect_identical(io$release_download("manifest.json", out), 0L)
  expect_identical(io$release_download("c2d4u-downloads-*.db", out), 0L)
  prev <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_silent(verify_release(out, prev))
  expect_identical(prev$history_method, "summed")
  expect_identical(prev$counted_through, "2026-09-14")
  expect_identical(prev$summary$latest_date, "2026-09-14")
  expect_identical(prev$unfetched_releases, 0L)
  expect_length(prev$package_edges, 0L)
  expect_true(prev$complete)
  expect_setequal(names(prev$shards), setdiff(list.files(pub), "manifest.json"))
  for (f in names(prev$shards)) expect_match(prev$shards[[f]]$sha256, "^[0-9a-f]{64}$")
  expect_identical(prev$shards[["c2d4u-downloads-2024.db"]]$sum, 6L)
  expect_identical(prev$db_sha256, file_sha256(file.path(pub, "c2d4u-downloads-summary.db")))
  expect_identical(m$counted_through, "2026-09-14")
})

test_that("seed_release embeds the roster and a summary anchored on counted_through", {
  pub <- withr::local_tempdir()
  fx <- seed_fixture()
  seed_release(pub, fx$roster, fx$daily, list(counted_through = "2026-09-14"))
  rp <- file.path(pub, "c2d4u-downloads-recent.db")
  expect_identical(load_releases(rp)$pub_id, fx$roster$pub_id)
  s <- load_summary(file.path(pub, "c2d4u-downloads-summary.db"))
  zoo <- s[s$package == "zoo", ]
  expect_identical(zoo$cnt_total, 19L)
  expect_identical(zoo$total_30d, 9L)   # 2026-09-10 is within 30 days of 09-14
  expect_identical(load_summary(rp)$cnt_total, s$cnt_total)
  # the recent shard holds RECENT_WINDOW_DAYS back from counted_through
  recent <- load_daily(rp)
  expect_setequal(recent$date, c("2025-12-31", "2026-09-10"))
})

test_that("seed_release defaults counted_through to the last day of data", {
  pub <- withr::local_tempdir()
  fx <- seed_fixture()
  m <- seed_release(pub, fx$roster, fx$daily)
  expect_identical(m$counted_through, "2026-09-10")
  expect_identical(m$last_checked, "2026-09-12T01:00:00Z")
})

test_that("seed_release applies manifest_extra, including removing contract fields", {
  pub <- withr::local_tempdir()
  fx <- seed_fixture()
  roster <- fx$roster; roster$done[3] <- 0L
  m <- seed_release(pub, roster, fx$daily,
                    list(counted_through = "2026-09-14", package_edges = list(abc = "2026-08-01")))
  expect_identical(m$unfetched_releases, 1L)
  expect_identical(m$package_edges, list(abc = "2026-08-01"))
  expect_false(m$complete)
  legacy <- seed_release(withr::local_tempdir(), fx$roster, fx$daily,
                         list(history_method = NULL, counted_through = NULL))
  expect_null(legacy$history_method)
  expect_null(legacy$counted_through)
  expect_identical(legacy$summary$latest_date, "2026-09-10")
})

test_that("seed_release refuses daily rows after counted_through", {
  fx <- seed_fixture()
  expect_error(seed_release(withr::local_tempdir(), fx$roster, fx$daily,
                            list(counted_through = "2026-09-01")), "after counted_through")
})
