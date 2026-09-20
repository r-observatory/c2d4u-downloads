# run_merge folds the fetched parts into one published release: it sums every
# release's downloads per (package, date), counts only days settled when the
# parts were fetched, and writes the summed-history manifest.

test_that("run_merge folds shard partials into year shards + summary with identity_state", {
  out <- withr::local_tempdir(); parts <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"), states = c(ggplot2 = "archived"))
  rp <- run_enumerate(bf_io(mk_pages(), ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  file.copy(run_fetch_shard(bf_io(mk_pages()), out, rp, i = 0L, N = 1L), parts)
  utils::capture.output(res <- run_merge(bf_io(mk_pages()), out, parts, N = 1L, roster_path = rp))
  expect_true("c2d4u-downloads-2024.db" %in% res$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package,cnt_total,origin,identity_state FROM c2d4u_downloads_summary")
  expect_identical(s$cnt_total, 40L)
  expect_identical(s$origin, "cran")
  expect_identical(s$identity_state, "archived")
})

test_that("run_merge sums a package's releases fetched in different shards", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo"), version = c("1", "2"))
  rows <- rel_rows("r-cran-zoo", c("1", "2"), "2024-02-01", c(40, 2))
  bf_merge(d, roster, rows, N = 2L)
  got <- out_daily(file.path(d, "out"))
  expect_identical(got$count, 42L)
  expect_identical(out_summary(file.path(d, "out"))$cnt_total, 42L)
})

test_that("run_merge sums across shard 0 and shard 10 whatever order the parts list in", {
  # list.files puts shard-10 between shard-1 and shard-2; the sum must not care.
  d <- withr::local_tempdir()
  bins <- c("r-cran-zoo", sprintf("r-cran-p%02d", 2:10), "r-cran-zoo")
  roster <- mk_roster(bins, version = c("1", rep("1", 9), "2"))
  expect_identical(shard_rows(11L, 10L, 11L), 11L)   # zoo 2 lands in shard 10
  rows <- rbind(rel_rows("r-cran-zoo", "1", "2024-02-01", 40),
                rel_rows(bins[2:10], "1", "2024-02-01", 1),
                rel_rows("r-cran-zoo", "2", "2024-02-01", 2))
  bf_merge(d, roster, rows, N = 11L)
  got <- out_daily(file.path(d, "out"))
  expect_identical(got$count[got$package == "zoo"], 42L)
  expect_identical(nrow(got), 10L)
})

test_that("a package with releases in three shards gets their sum on every day (incident regression)", {
  # The first-wins merge kept one shard's partial per package-day, so a package
  # with three releases in three shards published one release's count.
  d <- withr::local_tempdir()
  roster <- mk_roster(rep("r-cran-rcpp", 3), version = c("1", "2", "3"))
  rows <- rbind(rel_rows("r-cran-rcpp", "1", c("2025-03-01", "2025-03-02"), c(5, 1)),
                rel_rows("r-cran-rcpp", "2", c("2025-03-01", "2025-03-03"), c(7, 2)),
                rel_rows("r-cran-rcpp", "3", "2025-03-01", 11))
  bf_merge(d, roster, rows, N = 3L)
  got <- out_daily(file.path(d, "out"))
  expect_identical(got$date, c("2025-03-01", "2025-03-02", "2025-03-03"))
  expect_identical(got$count, c(23L, 1L, 2L))
  for (v in c("1", "2", "3")) {
    r <- rows[rows$version == v, ]
    expect_true(all(got$count[match(r$day, got$date)] >= r$count))
  }
  expect_identical(sum(got$count), sum(rows$count))
})

test_that("run_merge counts through the earliest fetch less the settled lag, not its own clock", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  rows <- rbind(rel_rows("r-cran-zoo", "1.0", c("2026-06-03", "2026-06-04", "2026-07-03", "2026-07-04"),
                         c(1, 2, 3, 4)),
                rel_rows("r-cran-abc", "1.0", c("2026-07-03", "2026-07-05"), c(10, 20)))
  # the merge runs 60 days after the fetch
  io <- fake_io(file.path(d, "pub"), now = as.POSIXct("2026-09-04 12:00:00", tz = "UTC"))
  res <- bf_merge(d, roster, rows, N = 2L, io = io,
                  fetched_at = c("2026-07-06 01:00:00", "2026-07-05 23:30:00"))
  through <- "2026-07-03"   # 2026-07-05 - SETTLED_LAG_DAYS
  expect_identical(res$manifest$counted_through, through)
  expect_identical(res$manifest$summary$latest_date, through)
  got <- out_daily(file.path(d, "out"))
  expect_true(all(got$date <= through))
  expect_identical(got$count, c(10L, 1L, 2L, 3L))
  # the summary is anchored on counted_through: its 30 days start 2026-06-03
  s <- out_summary(file.path(d, "out"))
  expect_identical(s$total_30d[s$package == "zoo"], 6L)
  expect_identical(s$last_date[s$package == "zoo"], through)
  expect_identical(s$total_30d[s$package == "abc"], 10L)
})

test_that("run_merge refuses a part without a fetch record and writes nothing", {
  d <- withr::local_tempdir()
  roster <- mk_roster("r-cran-zoo")
  mk_parts(file.path(d, "parts"), roster, rel_rows("r-cran-zoo", "1.0", "2024-02-01", 1), N = 1L)
  part <- file.path(d, "parts", sprintf("%s-shard-0.db", SHARD_PREFIX))
  con <- DBI::dbConnect(RSQLite::SQLite(), part)
  DBI::dbExecute(con, sprintf("DROP TABLE %s", PART_META_TABLE)); DBI::dbDisconnect(con)
  out <- file.path(d, "out")
  rp <- write_roster(file.path(d, ROSTER_FILE), roster)
  expect_error(run_merge(fake_io(file.path(d, "pub")), out, file.path(d, "parts"), N = 1L,
                         roster_path = rp),
               "shard-0.db has no fetch record")
  expect_length(list.files(out), 0L)
})

test_that("run_merge publishes the summed-history contract", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo", "r-cran-abc"), version = c("1", "2", "1"))
  rows <- rbind(rel_rows("r-cran-zoo", "1", c("2025-12-31", "2026-07-01"), c(4, 5)),
                rel_rows("r-cran-zoo", "2", "2026-07-01", 6),
                rel_rows("r-cran-abc", "1", "2024-05-01", 2))
  now <- as.POSIXct("2026-07-06 04:10:00", tz = "UTC")
  res <- bf_merge(d, roster, rows, N = 2L, io = fake_io(file.path(d, "pub"), now = now))
  out <- file.path(d, "out")
  m <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  published <- c("c2d4u-downloads-2024.db", "c2d4u-downloads-2025.db", "c2d4u-downloads-2026.db",
                 "c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db")
  expect_identical(unlist(m$changed_shards), published)
  expect_identical(res$changed_shards, published)
  expect_identical(readLines(file.path(out, UPLOAD_LIST)), published)
  expect_setequal(names(m$shards), published)
  for (f in published) expect_identical(m$shards[[f]]$sha256, file_sha256(file.path(out, f)))
  expect_identical(m$shards[["c2d4u-downloads-2026.db"]]$sum, 11L)
  expect_silent(verify_release(out, m))
  expect_identical(m$history_method, "summed")
  expect_identical(m$counted_through, "2026-07-04")
  expect_identical(m$summary$latest_date, "2026-07-04")
  expect_identical(m$summary$releases, 3L)
  expect_identical(m$summary$packages, 2L)
  expect_identical(m$package_edges, setNames(list(), character(0)))
  expect_identical(m$unfetched_releases, 0L)
  expect_identical(m$detected_gaps, list())
  expect_identical(m$known_gaps[[1]]$from, KNOWN_SOURCE_GAPS[[1]]$from)
  expect_identical(m$coverage_scope, COVERAGE_SCOPE)
  expect_identical(m$source_kind, "launchpad")
  expect_identical(m$last_checked, "2026-07-06T04:10:00Z")
  expect_identical(m$db_sha256, file_sha256(file.path(out, "c2d4u-downloads-summary.db")))
  expect_true(m$complete)
  expect_true(file.exists(file.path(out, "release_notes.md")))
  # the recent shard carries the merged roster, cnt_total unset
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(nrow(rel), 3L)
  expect_true(all(is.na(rel$cnt_total)))
})

test_that("run_merge counts unfetched releases and is not complete while any remain", {
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%03d", 1:200)
  roster <- mk_roster(bins, done = c(0L, rep(1L, 199)))
  res <- bf_merge(d, roster, rel_rows(bins[-1], "1.0", "2026-07-01", 1), N = 2L)
  expect_identical(res$manifest$unfetched_releases, 1L)
  expect_false(res$manifest$complete)
})

test_that("run_merge counts releases answered empty once apart from unfetched ones", {
  # 30 of 200 releases answered one empty page: they were fetched, so they do
  # not count toward the unfetched floor, but the release is not complete
  # until the monthly update has asked again
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%03d", 1:200)
  roster <- mk_roster(bins, done = c(0L, rep(2L, 30), rep(1L, 169)))
  msgs <- testthat::capture_messages(
    res <- bf_merge(d, roster, rel_rows(bins[-(1:31)], "1.0", "2026-07-01", 1), N = 2L))
  m <- res$manifest
  expect_identical(m$unfetched_releases, 1L)
  expect_identical(m$empty_once_releases, 30L)
  expect_false(m$complete)
  expect_true(any(grepl("1 of 200 releases unfetched, 30 answered empty once", msgs)))
  rel <- load_releases(file.path(d, "out", "c2d4u-downloads-recent.db"))
  expect_identical(sum(rel$done == 2L), 30L)
  # with every release settled and nothing held, the release is complete
  d2 <- withr::local_tempdir()
  roster2 <- mk_roster(bins[1:3], done = 1L)
  res2 <- suppressMessages(bf_merge(d2, roster2, rel_rows(bins[1:3], "1.0", "2026-07-01", 1), N = 1L))
  expect_identical(res2$manifest$empty_once_releases, 0L)
  expect_true(res2$manifest$complete)
})

# --- the end of the history: the monthly update's stall rule ----------------

test_that("run_merge holds counted_through before a trailing run of days with no downloads", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  days <- format(seq(as.Date("2026-08-01"), as.Date("2026-09-10"), by = "day"))
  rows <- rbind(rel_rows("r-cran-zoo", "1.0", days, 2), rel_rows("r-cran-abc", "1.0", "2026-09-05", 3))
  txt <- capture.output(res <- suppressMessages(
    bf_merge(d, roster, rows, N = 2L, fetched_at = "2026-09-20 01:00:00", show_output = TRUE)))
  expect_true(any(grepl(paste0("^::warning::c2d4u backfill: Launchpad reported no downloads from ",
                               "2026-09-11 to 2026-09-18; counted_through stops at 2026-09-10"), txt)))
  m <- res$manifest
  expect_identical(m$counted_through, "2026-09-10")
  expect_identical(m$summary$latest_date, "2026-09-10")
  expect_length(m$detected_gaps, 0L)
  expect_null(m$refetch_from)
  # the summary is anchored on the day it counts through: 08-11 .. 09-10
  s <- out_summary(file.path(d, "out"))
  expect_identical(s$total_30d[s$package == "zoo"], 62L)
  expect_identical(s$last_date[s$package == "zoo"], "2026-09-10")
})

test_that("run_merge counts a quiet stretch past STALL_MAX_DAYS as stopped, from its first quiet day", {
  d <- withr::local_tempdir()
  roster <- mk_roster("r-cran-zoo")
  rows <- rel_rows("r-cran-zoo", "1.0", c("2026-01-09", "2026-01-10"), c(1, 2))
  txt <- capture.output(res <- suppressMessages(
    bf_merge(d, roster, rows, N = 1L, fetched_at = "2026-09-20 01:00:00", show_output = TRUE)))
  expect_true(any(grepl("^::warning::c2d4u backfill: .*no downloads from 2026-01-11 to 2026-09-18", txt)))
  m <- res$manifest
  expect_identical(m$counted_through, "2026-09-18")
  expect_identical(m$detected_gaps, list(list(from = "2026-01-11", to = "2026-09-18", kind = "stopped")))
  expect_identical(m$refetch_from, "2026-01-11")
})

test_that("run_merge keeps the published gaps and judges a stretch counted as stopped again", {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  zoo <- format(seq(as.Date("2026-06-01"), as.Date("2026-09-20"), by = "day"))
  gaps <- list(list(from = "2026-02-01", to = "2026-02-05"),
               list(from = "2026-09-21", to = "2027-01-01", kind = "stopped"))
  merge_filled <- function(d, fill) {
    seed_release(file.path(d, "pub"), roster,
                 data.frame(package = c(rep("zoo", length(zoo)), "abc"), date = c(zoo, "2026-08-10"),
                            count = c(rep(1L, length(zoo)), 3L)),
                 list(counted_through = "2027-01-01", detected_gaps = gaps, refetch_from = "2026-09-21"))
    rows <- rbind(rel_rows("r-cran-zoo", "1.0", c(zoo, fill), 1),
                  rel_rows("r-cran-abc", "1.0", "2026-08-10", 3))
    txt <- capture.output(res <- suppressMessages(
      bf_merge(d, roster, rows, N = 2L, fetched_at = "2027-01-20 01:00:00", show_output = TRUE)))
    c(res$manifest, list(txt = txt))
  }
  # Launchpad filled in six days of the stretch and the quiet since is past
  # STALL_MAX_DAYS: the stretch is trimmed, the earlier gap kept, and the old
  # first day stays the floor so the next update fetches the filled days again
  m <- merge_filled(withr::local_tempdir(), format(seq(as.Date("2026-10-05"), as.Date("2026-10-10"),
                                                       by = "day")))
  expect_identical(m$counted_through, "2027-01-18")
  expect_identical(m$detected_gaps, list(gaps[[1]], list(from = "2026-09-21", to = "2026-10-04"),
                                         list(from = "2026-10-11", to = "2027-01-18", kind = "stopped")))
  expect_identical(m$refetch_from, "2026-09-21")
  # filled in recently enough that the quiet since is a stall that began
  # before the published counted_through, which does not move back: the
  # published gaps stay as they are until that quiet counts as stopped
  m2 <- merge_filled(withr::local_tempdir(), format(seq(as.Date("2026-12-20"), as.Date("2026-12-25"),
                                                        by = "day")))
  expect_identical(m2$counted_through, "2027-01-01")
  expect_identical(m2$detected_gaps, gaps)
  expect_identical(m2$refetch_from, "2026-09-21")
  expect_true(any(grepl(paste0("^::warning::.*no downloads from 2026-12-26 to 2027-01-18; ",
                               "counted_through stays at 2027-01-01"), m2$txt)))
})

test_that("run_merge keeps the refetch floor of a package the published release held", {
  # abc and cat were held, so this backfill is the first run to store the
  # days their floors cover. dog's floor lies inside its revision window, and
  # zoo was refreshed when its floor was set: the backfill is its second look
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc", "r-cran-cat", "r-cran-dog"))
  zoo <- format(seq(as.Date("2026-06-01"), as.Date("2027-01-18"), by = "day"))
  one <- c("r-cran-abc", "r-cran-cat", "r-cran-dog")
  rows <- rbind(rel_rows("r-cran-zoo", "1.0", zoo, 1), rel_rows(one, "1.0", "2026-08-10", 3))
  published <- aggregate_counts(rows[rows$day <= "2027-01-01", ], roster)
  seed_release(file.path(d, "pub"), roster, published,
               list(counted_through = "2027-01-01",
                    package_edges = list(abc = "2026-12-01", cat = "2026-12-01", dog = "2026-11-01"),
                    refetch_from = "2026-10-15",
                    package_refetch_from = list(abc = "2026-09-01", zoo = "2026-08-20")))
  m <- suppressMessages(bf_merge(d, roster, rows, N = 2L, fetched_at = "2027-01-20 01:00:00"))$manifest
  expect_identical(m$counted_through, "2027-01-18")
  expect_length(m$package_edges, 0L)
  expect_null(m$refetch_from)
  expect_identical(m$package_refetch_from, list(abc = "2026-09-01", cat = "2026-10-15"))
})

# --- the publish floor: nothing is written unless every check passes --------

test_that("run_merge refuses to merge without every part", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c"))
  mk_parts(file.path(d, "parts"), roster, rel_rows(roster$binary_name, "1.0", "2026-07-01", 1), N = 3L)
  unlink(file.path(d, "parts", sprintf("%s-shard-1.db", SHARD_PREFIX)))
  rp <- write_roster(file.path(d, ROSTER_FILE), roster)
  out <- file.path(d, "out")
  expect_error(run_merge(fake_io(file.path(d, "pub")), out, file.path(d, "parts"), N = 3L, roster_path = rp),
               "missing c2d4u-downloads-shard-1.db")
  expect_length(list.files(out), 0L)
})

test_that("run_merge refuses a part it was not told to expect", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c"))
  mk_parts(file.path(d, "parts"), roster, rel_rows(roster$binary_name, "1.0", "2026-07-01", 1), N = 3L)
  rp <- write_roster(file.path(d, ROSTER_FILE), roster)
  expect_error(run_merge(fake_io(file.path(d, "pub")), file.path(d, "out"), file.path(d, "parts"),
                         N = 2L, roster_path = rp),
               "unexpected c2d4u-downloads-shard-2.db")
})

test_that("run_merge refuses a part whose fetch record names another shard", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-a", "r-cran-b"))
  parts <- mk_parts(file.path(d, "parts"), roster, rel_rows(roster$binary_name, "1.0", "2026-07-01", 1),
                    N = 2L)
  f <- function(i) file.path(parts, sprintf("%s-shard-%d.db", SHARD_PREFIX, i))
  file.rename(f(0), f(9)); file.rename(f(1), f(0)); file.rename(f(9), f(1))
  rp <- write_roster(file.path(d, ROSTER_FILE), roster)
  expect_error(run_merge(fake_io(file.path(d, "pub")), file.path(d, "out"), parts, N = 2L, roster_path = rp),
               "shard-0.db says it is shard 1 of 2")
})

test_that("run_merge refuses parts whose rosters do not add up to the enumerated roster", {
  d <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-a", "r-cran-b"))
  enumerated <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c"))
  expect_error(bf_merge(d, roster, rel_rows(roster$binary_name, "1.0", "2026-07-01", 1), N = 2L,
                        enumerated = enumerated),
               "2 roster rows, but the enumerated roster has 3")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge refuses an empty roster", {
  d <- withr::local_tempdir()
  expect_error(bf_merge(d, mk_roster(character(0)), rel_rows(character(0), character(0), character(0),
                                                             integer(0)), N = 1L),
               "roster is empty")
})

test_that("run_merge refuses to publish with more than UNFETCHED_MAX_FRAC unfetched", {
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%02d", 1:20)
  roster <- mk_roster(bins, done = c(0L, 0L, 0L, rep(1L, 17)))   # 15% unfetched
  expect_error(bf_merge(d, roster, rel_rows(bins[-(1:3)], "1.0", "2026-07-01", 1), N = 2L),
               "3 of 20 releases \\(15.0%\\) are unfetched")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge publishes with a GitHub warning above UNFETCHED_WARN_FRAC unfetched", {
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%02d", 1:20)
  roster <- mk_roster(bins, done = c(0L, rep(1L, 19)))            # 5% unfetched
  out <- capture.output(res <- bf_merge(d, roster, rel_rows(bins[-1], "1.0", "2026-07-01", 1), N = 2L,
                                        show_output = TRUE),
                        type = "output")
  expect_true(any(grepl("^::warning::.*1 of 20 releases \\(5.0%\\) are unfetched", out)))
  expect_identical(res$manifest$unfetched_releases, 1L)
  expect_true(file.exists(file.path(d, "out", "manifest.json")))
})

test_that("run_merge publishes without a warning below UNFETCHED_WARN_FRAC unfetched", {
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%03d", 1:200)
  roster <- mk_roster(bins, done = c(0L, rep(1L, 199)))           # 0.5% unfetched
  out <- capture.output(bf_merge(d, roster, rel_rows(bins[-1], "1.0", "2026-07-01", 1), N = 2L,
                                 show_output = TRUE),
                        type = "output")
  expect_false(any(grepl("::warning::", out)))
})

# A published release of n one-release packages, each with 10 downloads on
# 2026-05-01, counted through 2026-05-05, and the same history re-fetched.
dom_fixture <- function(d, n = 150L, manifest_extra = list(counted_through = "2026-05-05")) {
  bins <- sprintf("r-cran-p%03d", seq_len(n))
  roster <- mk_roster(bins, last_day = "2026-05-01")
  seed_release(file.path(d, "pub"), roster,
               data.frame(package = sub("^r-cran-", "", bins), date = "2026-05-01", count = 10L,
                          stringsAsFactors = FALSE),
               manifest_extra)
  list(roster = roster, bins = bins, rows = rel_rows(bins, "1.0", "2026-05-01", 10))
}

test_that("run_merge replaces a published release its history dominates", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  rows <- rbind(fx$rows, rel_rows(fx$bins[1], "1.0", "2026-06-01", 3))
  expect_message(res <- bf_merge(d, fx$roster, rows, N = 2L), "0 of 150 packages")
  expect_identical(res$manifest$summary$packages, 150L)
})

test_that("run_merge stops when more than DOMINANCE_MAX_FRAC of packages count fewer downloads", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  rows <- fx$rows; rows$count[1:2] <- 9L                           # 2 of 150 is 1.3%
  expect_error(bf_merge(d, fx$roster, rows, N = 2L),
               "2 of 150 packages .* fewer downloads through 2026-05-05.*p001 \\(9 < 10\\), p002 \\(9 < 10\\)")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge lists but tolerates up to DOMINANCE_MAX_FRAC of packages counting fewer", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  rows <- fx$rows; rows$count[1] <- 9L                             # 1 of 150 is 0.7%
  expect_message(bf_merge(d, fx$roster, rows, N = 2L), "1 of 150 packages .*p001 \\(9 < 10\\)")
  expect_true(file.exists(file.path(d, "out", "manifest.json")))
})

test_that("run_merge compares only the days through the published counted_through", {
  # downloads after the published edge cannot make up for fewer before it
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  rows <- rbind(fx$rows, rel_rows(fx$bins[1], "1.0", "2026-05-06", 50))
  rows$count[1] <- 9L
  expect_message(bf_merge(d, fx$roster, rows, N = 2L), "p001 \\(9 < 10\\)")
})

test_that("run_merge falls back to the published latest_date as the edge of a legacy release", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d, manifest_extra = list(history_method = NULL, counted_through = NULL))
  rows <- rbind(fx$rows, rel_rows(fx$bins[1], "1.0", "2026-05-03", 50))
  rows$count[1] <- 9L
  expect_message(bf_merge(d, fx$roster, rows, N = 2L), "through 2026-05-01.*p001 \\(9 < 10\\)")
})

test_that("run_merge refuses parts that count through an earlier day than the published release", {
  # a merge job re-run after a monthly update published: its parts were
  # fetched before that update, so they settle an earlier day, and every
  # package here counts as many downloads as before, so dominance passes
  d <- withr::local_tempdir()
  fx <- dom_fixture(d, manifest_extra = list(counted_through = "2026-08-01"))
  err <- expect_error(bf_merge(d, fx$roster, fx$rows, N = 2L, fetched_at = "2026-07-06 01:00:00"),
                      "predate the published release")
  expect_match(conditionMessage(err), "2026-07-04.*2026-08-01")
  expect_match(conditionMessage(err), "whole backfill.yml.*not just its merge job")
  expect_length(list.files(file.path(d, "out")), 0L)
  # parts settled on the published day itself still replace it
  d2 <- withr::local_tempdir()
  fx2 <- dom_fixture(d2, manifest_extra = list(counted_through = "2026-07-04"))
  res <- suppressMessages(bf_merge(d2, fx2$roster, fx2$rows, N = 2L, fetched_at = "2026-07-06 01:00:00"))
  expect_identical(res$manifest$counted_through, "2026-07-04")
})

test_that("run_merge stops on fewer releases than the published release", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  expect_error(bf_merge(d, fx$roster[-150, ], fx$rows[-150, ], N = 2L),
               "149 releases, fewer than the 150 of the published release")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge stops on fewer packages than the published release", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  expect_error(bf_merge(d, fx$roster, fx$rows[-150, ], N = 2L),
               "149 packages, fewer than the 150 of the published release")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge stops when it would leave a published year shard behind", {
  d <- withr::local_tempdir()
  roster <- mk_roster("r-cran-zoo")
  seed_release(file.path(d, "pub"), roster,
               data.frame(package = "zoo", date = c("2025-06-01", "2026-05-01"), count = c(5L, 10L)))
  expect_error(bf_merge(d, roster, rel_rows("r-cran-zoo", "1.0", "2026-05-01", 20), N = 1L),
               "c2d4u-downloads-2025.db")
  expect_length(list.files(file.path(d, "out")), 0L)
})

test_that("run_merge says when there is no published release to compare against", {
  d <- withr::local_tempdir()
  expect_message(bf_merge(d, mk_roster("r-cran-zoo"), rel_rows("r-cran-zoo", "1.0", "2026-05-01", 1),
                          N = 1L),
                 "no published release")
})

test_that("run_merge stops when the published release cannot be read", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "pub"))
  writeLines("{}", file.path(d, "pub", "manifest.json"))          # no summary to compare with
  roster <- mk_roster("r-cran-zoo")
  expect_error(bf_merge(d, roster, rel_rows("r-cran-zoo", "1.0", "2026-05-01", 20), N = 1L),
               "could not be downloaded")
  expect_length(list.files(file.path(d, "out")), 0L)
})

# A release that lost one asset to an upload that deleted it and then failed
# (gh release upload --clobber deletes first). The release itself still exists.
lost_asset_io <- function(d, lost) {
  unlink(file.path(d, "pub", lost))
  io <- fake_io(file.path(d, "pub"))
  io$release_exists <- function() TRUE
  io
}

test_that("run_merge checks against the recent shard's summary when the summary DB is lost", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  io <- lost_asset_io(d, "c2d4u-downloads-summary.db")
  expect_message(res <- bf_merge(d, fx$roster, fx$rows, N = 2L, io = io), "0 of 150 packages")
  expect_identical(res$manifest$summary$packages, 150L)
  # and it still refuses a history that lost downloads
  d2 <- withr::local_tempdir()
  fx2 <- dom_fixture(d2)
  rows <- fx2$rows; rows$count[1:2] <- 9L
  expect_error(bf_merge(d2, fx2$roster, rows, N = 2L, io = lost_asset_io(d2, "c2d4u-downloads-summary.db")),
               "2 of 150 packages .* fewer downloads")
})

test_that("run_merge checks against the published summary and roster when the manifest is lost", {
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  io <- lost_asset_io(d, "manifest.json")
  msgs <- testthat::capture_messages(res <- bf_merge(d, fx$roster, fx$rows, N = 2L, io = io))
  expect_true(any(grepl("no manifest.json", msgs)))
  expect_true(any(grepl("0 of 150 packages", msgs)))
  expect_true(file.exists(file.path(d, "out", "manifest.json")))
  # the roster floor comes from the published recent shard
  d2 <- withr::local_tempdir()
  fx2 <- dom_fixture(d2)
  expect_error(bf_merge(d2, fx2$roster[-150, ], fx2$rows[-150, ], N = 2L,
                        io = lost_asset_io(d2, "manifest.json")),
               "149 releases, fewer than the 150 of the published release")
})

test_that("run_merge compares through the summary's own last day when it is past the manifest's", {
  # a publish torn after its summary landed: the summary counts through
  # 2026-06-01, the manifest still says 2026-05-05
  d <- withr::local_tempdir()
  fx <- dom_fixture(d)
  newer <- file.path(d, "newer")
  daily <- rbind(data.frame(package = sub("^r-cran-", "", fx$bins), date = "2026-05-01", count = 10L),
                 data.frame(package = sub("^r-cran-", "", fx$bins), date = "2026-06-01", count = 5L))
  seed_release(newer, fx$roster, daily, list(counted_through = "2026-06-01"))
  file.copy(file.path(newer, "c2d4u-downloads-summary.db"), file.path(d, "pub"), overwrite = TRUE)
  rows <- rbind(fx$rows, rel_rows(fx$bins, "1.0", "2026-06-01", 5))
  expect_message(bf_merge(d, fx$roster, rows, N = 2L), "0 of 150 packages .* through 2026-06-01")
})
