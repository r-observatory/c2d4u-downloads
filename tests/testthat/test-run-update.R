# run_update and its CLI. fake_io, seed_release, mk_roster, counts_json and
# publish live in helper-setup.R. Launchpad is simulated from release-level
# truth (lp_rows): the fake answers each getDownloadCounts url with the rows
# inside its start_date and end_date, newest first, paged as Launchpad pages.

t_at <- function(day, hms = "06:00:00") as.POSIXct(paste(day, hms), tz = "UTC")

# The standard clock: E = 2026-09-01 is the published counted_through, so a run
# on 2026-10-03 settles through T = 2026-10-01 and re-fetches from S = 2026-08-02.
E_STD   <- "2026-09-01"
NOW_STD <- t_at("2026-10-03")

lp_rows <- function(pub_id, day, count)
  data.frame(pub_id = as.integer(pub_id), day = as.character(day), count = as.integer(count),
             stringsAsFactors = FALSE)

url_param <- function(url, key) {
  rx <- sprintf("[?&]%s=([^&]*)", gsub(".", "\\.", key, fixed = TRUE))
  m <- regmatches(url, regexec(rx, url))[[1]]
  if (length(m) == 2L) m[2] else NA_character_
}
url_pub <- function(url) as.integer(sub("^.*/\\+binarypub/([0-9]+)\\?.*$", "\\1", url))

# One getDownloadCounts page for `url`, answered from `lp`, `page` rows a page.
lp_answer <- function(url, lp, roster, page = PAGE_SIZE) {
  pub <- url_pub(url)
  sd <- url_param(url, "start_date"); ed <- url_param(url, "end_date")
  st <- suppressWarnings(as.integer(url_param(url, "ws.start")))
  if (is.na(st)) st <- 0L
  r <- lp[lp$pub_id == pub, , drop = FALSE]
  if (!is.na(sd)) r <- r[r$day >= sd, , drop = FALSE]
  if (!is.na(ed)) r <- r[r$day <= ed, , drop = FALSE]
  r <- r[order(r$day, decreasing = TRUE), , drop = FALSE]
  k <- seq_len(nrow(r))
  take <- r[k > st & k <= st + page, , drop = FALSE]
  nl <- if (st + page < nrow(r))
    paste0(sub("&ws\\.start=[0-9]+$", "", url), "&ws.start=", st + page) else NULL
  i <- match(pub, roster$pub_id)
  counts_json(roster$binary_name[i], roster$version[i], take$day, take$count, next_link = nl,
              total_size = nrow(r), start = st)
}

# fake_io over the release in `pub`, with Launchpad answering from `lp`.
# pool_fail and serial_fail are predicates on a url: TRUE fails that request in
# the pool (io$fetch_many) or serially (io$fetch). io$log$seq records every
# request in order as "many <url>" or "fetch <url>"; io$log$batches holds the
# urls of each fetch_many call and io$log$deadlines the deadline it was given.
# on_many(urls) runs before each pooled call, so a test can move its clock.
lp_io <- function(pub, roster, lp, now = NOW_STD, page = PAGE_SIZE,
                  pool_fail = function(u) FALSE, serial_fail = function(u) FALSE,
                  on_many = function(urls) NULL, ...) {
  io <- fake_io(pub, now = now, ...)
  log <- new.env(parent = emptyenv())
  log$seq <- character(0); log$batches <- list(); log$deadlines <- list()
  io$fetch <- function(url) {
    log$seq <- c(log$seq, paste("fetch", url))
    if (serial_fail(url)) NULL else lp_answer(url, lp, roster, page)
  }
  io$fetch_many <- function(urls, deadline = Inf) {
    on_many(urls)
    log$seq <- c(log$seq, paste("many", urls))
    log$batches[[length(log$batches) + 1L]] <- urls
    log$deadlines[[length(log$deadlines) + 1L]] <- deadline
    lapply(urls, function(u) if (pool_fail(u)) NULL else lp_answer(u, lp, roster, page))
  }
  io$log <- log
  io
}
requested <- function(io) sub("^(many|fetch) ", "", io$log$seq)

# Seed `pub` with a summed release counted through E, as the backfill would
# publish it from `lp`: the history sums every fetched (done == 1) release's
# rows up to its package's edge (E, or the package's entry in `edges`), and
# each such release's last_day is its last row up to that edge. Releases with
# done == 0 are in the roster only. `extra` is merged over the manifest.
seed_world <- function(pub, roster, lp, E, edges = list(), extra = list()) {
  E <- as.character(E)
  edge <- rep(E, nrow(roster))
  m <- match(roster$package, names(edges))
  edge[!is.na(m)] <- unlist(edges, use.names = FALSE)[m[!is.na(m)]]
  done <- roster$done == 1L
  r <- lp[lp$pub_id %in% roster$pub_id[done], , drop = FALSE]
  r <- r[r$day <= edge[match(r$pub_id, roster$pub_id)], , drop = FALSE]
  last <- if (nrow(r)) tapply(r$day, r$pub_id, max) else character(0)
  roster$last_day <- ifelse(done, unname(last[as.character(roster$pub_id)]), NA_character_)
  daily <- if (nrow(r)) stats::aggregate(count ~ package + date, FUN = sum,
    data = data.frame(package = roster$package[match(r$pub_id, roster$pub_id)],
                      date = r$day, count = r$count, stringsAsFactors = FALSE))
    else data.frame(package = character(0), date = character(0), count = integer(0))
  daily$count <- as.integer(daily$count)
  seed_release(pub, roster, daily, c(list(package_edges = edges), extra), counted_through = E)
  invisible(roster)
}

# Package-day sums of `lp` over the roster's releases in `pubs`, through `through`.
truth_daily <- function(lp, roster, through, pubs = roster$pub_id) {
  r <- lp[lp$pub_id %in% pubs & lp$day <= through, , drop = FALSE]
  d <- stats::aggregate(count ~ package + date, FUN = sum,
    data = data.frame(package = roster$package[match(r$pub_id, roster$pub_id)],
                      date = r$day, count = r$count, stringsAsFactors = FALSE))
  d$count <- as.integer(d$count)
  d <- d[order(d$package, d$date), c("package", "date", "count")]
  rownames(d) <- NULL
  d
}

# The daily history held by the year shards in `dir`, sorted; only the
# packages in `pkg`, or all but those in `not`.
history_of <- function(dir, pkg = NULL, not = NULL) {
  fs <- list.files(dir, pattern = "^c2d4u-downloads-20[0-9]{2}\\.db$", full.names = TRUE)
  d <- do.call(rbind, lapply(fs, load_daily))
  if (!is.null(pkg)) d <- d[d$package %in% pkg, , drop = FALSE]
  if (!is.null(not)) d <- d[!d$package %in% not, , drop = FALSE]
  d <- d[order(d$package, d$date), c("package", "date", "count")]
  rownames(d) <- NULL
  d
}
cnt <- function(d, pkg, day) {
  x <- d$count[d$package == pkg & d$date == day]
  if (length(x)) x else 0L
}
read_manifest <- function(dir) jsonlite::fromJSON(file.path(dir, "manifest.json"), simplifyVector = FALSE)

# A stopped run leaves out_dir holding only the published assets it downloaded,
# byte for byte: no manifest, shard or release notes of its own.
expect_nothing_written <- function(out, pub) {
  got <- list.files(out)
  expect_true(all(got %in% list.files(pub)), info = paste(setdiff(got, list.files(pub)), collapse = ", "))
  expect_true("manifest.json" %in% got)
  for (f in got)
    expect_identical(unname(tools::md5sum(file.path(out, f))),
                     unname(tools::md5sum(file.path(pub, f))), info = f)
}

# run_update with its messages and stdout collected into the result.
run_q <- function(...) {
  msgs <- character(0)
  txt <- utils::capture.output(res <- withCallingHandlers(run_update(...),
    message = function(m) {
      msgs <<- c(msgs, sub("\n$", "", conditionMessage(m))); invokeRestart("muffleMessage")
    }))
  res$messages <- msgs; res$stdout <- txt
  res
}
quietly <- function(expr) suppressMessages(utils::capture.output(expr))

# Two zoo releases and one abc release. zoo v1 has a download every day from
# June through `through`, so no run of empty days appears in a window. zoo v2
# has 1 of zoo's 63 downloads in the standard overlap, a drop the regression
# guard tolerates, so losing it must be caught by holding the package.
std_world <- function(n_fill = 0L, through = "2026-11-02") {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo", "r-cran-abc"), version = c("1", "2", "1"))
  lp <- rbind(
    lp_rows(1, "2026-01-10", 100),
    lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date(through), by = "day")), 2),
    lp_rows(2, c("2026-08-15", "2026-09-15"), c(1, 6)),
    lp_rows(3, c("2026-08-20", "2026-09-20"), c(3, 8)))
  if (n_fill > 0L) {
    fid <- 100L + seq_len(n_fill)
    roster <- rbind(roster, mk_roster(sprintf("r-cran-fill%02d", seq_len(n_fill)), pub_id = fid))
    lp <- rbind(lp, lp_rows(fid, "2026-08-10", 3), lp_rows(fid, "2026-09-10", 1))
  }
  list(roster = roster, lp = lp)
}

fake_env <- function(...) {
  vals <- list(...)
  function(x, unset = "") if (is.null(vals[[x]])) unset else vals[[x]]
}

# --- preconditions ------------------------------------------------------------

test_that("a release that is not a summed history stops the update with nothing written", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD, extra = list(history_method = NULL, counted_through = NULL))
  io <- lp_io(pub, w$roster, w$lp)
  expect_error(run_q(io, out), "not a summed history.*run backfill.yml first")
  expect_nothing_written(out, pub)
  expect_length(io$log$seq, 0L)
})

test_that("a year shard that differs from its fingerprint (a torn publish) stops the update", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  export_shard(file.path(pub, "c2d4u-downloads-2026.db"),
               data.frame(package = "zoo", date = "2026-08-15", count = 1L))
  io <- lp_io(pub, w$roster, w$lp)
  expect_error(run_q(io, out), "torn or foreign release: c2d4u-downloads-2026.db .*run backfill.yml")
  expect_nothing_written(out, pub)
  expect_length(io$log$seq, 0L)
})

test_that("a failed download of an intact release asks for a re-run, not a backfill", {
  pub <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  for (pattern in c("c2d4u-downloads-20*.db", "c2d4u-downloads-summary.db")) {
    out <- withr::local_tempdir()
    io <- lp_io(pub, w$roster, w$lp)
    dl <- io$release_download
    io$release_download <- function(p, dir) if (identical(p, pattern)) 1L else dl(p, dir)
    err <- expect_error(run_q(io, out), "could not download", info = pattern)
    expect_match(conditionMessage(err), "re-run", info = pattern)
    expect_no_match(conditionMessage(err), "torn", info = pattern)
    expect_length(io$log$seq, 0L)
    expect_false(file.exists(file.path(out, UPLOAD_LIST)))
  }
})

test_that("a cold start stops with a pointer to backfill.yml", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  io <- lp_io(pub, mk_roster("r-cran-zoo"), lp_rows(1, "2026-09-01", 1))
  expect_error(run_q(io, out), "no published release.*run backfill.yml first")
  expect_length(io$log$seq, 0L)
  # a published release without a roster is a cold start too
  seed_release(pub, mk_roster(character(0)),
               data.frame(package = character(0), date = character(0), count = integer(0)),
               counted_through = E_STD)
  expect_error(run_q(io, out), "cold start.*run backfill.yml first")
  expect_length(io$log$seq, 0L)
})

test_that("protect-history aborts when recent shard cannot be downloaded", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  writeLines("{}", file.path(pub, "manifest.json"))  # release exists but no recent shard
  expect_error(run_update(fake_io(pub), out), "protect")
})

test_that("the monthly update has no full-rebuild mode and its CLI refuses one", {
  expect_false("force_full" %in% names(formals(run_update)))
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  io <- lp_io(pub, w$roster, w$lp)
  expect_error(update_cli(out, env = fake_env(C2D4U_FORCE_REBUILD = "true"), io = io),
               "C2D4U_FORCE_REBUILD.*backfill.yml")
  expect_length(io$log$seq, 0L)
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("nothing settled past counted_through is a no-op that publishes nothing", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, "2026-10-01")
  io <- lp_io(pub, w$roster, w$lp, now = t_at("2026-10-03"))   # T = 2026-10-01 = E
  res <- run_q(io, out)
  expect_false(res$publish)
  expect_length(res$changed_shards, 0L)
  expect_length(io$log$seq, 0L)
  expect_nothing_written(out, pub)
})

test_that("the CLI marks a no-op run skip-publish and passes the deadline on", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, "2026-10-01")
  io <- lp_io(pub, w$roster, w$lp, now = t_at("2026-10-03"))
  quietly(res <- update_cli(out, env = fake_env(), io = io))
  expect_false(res$publish)
  expect_true(file.exists(file.path(out, "skip-publish")))

  out2 <- withr::local_tempdir()
  writeLines("stale", file.path(out2, "skip-publish"))
  io2 <- lp_io(pub, w$roster, w$lp, now = t_at("2026-11-03"))
  quietly(res2 <- update_cli(out2, env = fake_env(C2D4U_DEADLINE_MIN = "100"), io = io2))
  expect_true(res2$publish)
  expect_false(file.exists(file.path(out2, "skip-publish")))
  expect_true(all(vapply(io2$log$deadlines, function(d) identical(d, t_at("2026-11-03") + 100 * 60),
                         logical(1))))
  expect_error(update_cli(out2, env = fake_env(C2D4U_DEADLINE_MIN = "soon"), io = io2),
               "C2D4U_DEADLINE_MIN")
})

test_that("load_history reads every year shard and keeps a recent row no year shard holds", {
  d <- withr::local_tempdir()
  export_shard(file.path(d, "c2d4u-downloads-2025.db"),
               data.frame(package = "a", date = c("2025-01-01", "2025-12-30"), count = c(1L, 2L)))
  export_shard(file.path(d, "c2d4u-downloads-2026.db"),
               data.frame(package = "a", date = "2026-01-02", count = 3L))
  rp <- file.path(d, "c2d4u-downloads-recent.db")
  export_shard(rp, data.frame(package = c("a", "a", "b"), date = c("2025-12-30", "2026-01-02", "2026-01-02"),
                              count = c(2L, 3L, 4L)))
  h <- load_history(d, rp)
  h <- h[order(h$package, h$date), ]; rownames(h) <- NULL
  expect_identical(h, data.frame(package = c("a", "a", "a", "b"),
                                 date = c("2025-01-01", "2025-12-30", "2026-01-02", "2026-01-02"),
                                 count = c(1L, 2L, 3L, 4L), stringsAsFactors = FALSE))
  expect_identical(nrow(load_history(withr::local_tempdir(), tempfile())), 0L)
})

# --- candidates ---------------------------------------------------------------

test_that("never-downloaded and dormant releases are not fetched", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster(c("r-cran-never", "r-cran-old", "r-cran-edge"), pub_id = 5:7))
  # E - ACTIVE_WINDOW_DAYS is 2025-09-01: a release last downloaded that day is
  # still active, one downloaded the day before is dormant
  lp <- rbind(w$lp, lp_rows(6, "2025-08-31", 9), lp_rows(7, "2025-09-01", 4))
  seed_world(pub, roster, lp, E_STD)
  io <- lp_io(pub, roster, lp)
  res <- run_q(io, out)
  expect_setequal(unique(url_pub(requested(io))), c(1:3, 7L))
  s <- res$manifest$summary
  expect_identical(s$window_releases, 4L)
  expect_identical(s$full_history_releases, 0L)
  expect_identical(s$excluded_never_downloaded, 1L)
  expect_identical(s$excluded_dormant, 1L)
  expect_identical(s$active_releases, 4L)
  expect_true(any(grepl("4 window and 0 full-history.*1 never downloaded.*1 dormant", res$messages)))
  # the dormant package keeps its history
  expect_identical(cnt(history_of(out), "old", "2025-08-31"), 9L)
})

test_that("candidates hang off the data edge, so they do not age while it stands still", {
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c", "r-cran-d", "r-cran-e"),
                      last_day = c("2026-05-05", "2025-05-05", "2025-05-04", NA, NA),
                      done = c(1L, 1L, 1L, 1L, 0L))
  p1 <- update_plan(roster, "2026-05-05", list(), as.Date("2026-10-01"))
  p2 <- update_plan(roster, "2026-05-05", list(), as.Date("2026-11-01"))
  expect_identical(p1$w, 1:2)
  expect_identical(p1$f, 5L)
  expect_identical(p1$never, 4L)
  expect_identical(p1$dormant, 3L)
  expect_identical(p1[c("w", "f", "never", "dormant", "edge", "start")],
                   p2[c("w", "f", "never", "dormant", "edge", "start")])
  expect_identical(p1$start[1], as.Date("2026-04-05"))
  # a package's own edge moves both its activity cut and its window start
  p3 <- update_plan(roster, "2026-05-05", list(c = "2026-05-01"), as.Date("2026-10-01"))
  expect_identical(p3$w, 1:3)
  expect_identical(p3$start[3], as.Date("2026-04-01"))
})

test_that("a run on 2026-10-03 and one on 2026-11-03 fetch the same releases from a fixed edge", {
  pub <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c"))
  days <- format(c(seq(as.Date("2026-04-20"), as.Date("2026-05-05"), by = "day"),
                   seq(as.Date("2026-07-12"), as.Date("2026-11-02"), by = "day")))
  lp <- rbind(lp_rows(1, days, 1), lp_rows(2, "2025-05-04", 3))   # c never downloaded
  seed_world(pub, roster, lp, "2026-05-05")
  io1 <- lp_io(pub, roster, lp, now = t_at("2026-10-03"))
  io2 <- lp_io(pub, roster, lp, now = t_at("2026-11-03"))
  run_q(io1, withr::local_tempdir()); run_q(io2, withr::local_tempdir())
  expect_identical(unique(url_pub(requested(io1))), 1L)
  expect_identical(unique(url_pub(requested(io2))), 1L)
  expect_match(requested(io1), "start_date=2026-04-05&end_date=2026-10-01$")
  expect_match(requested(io2), "start_date=2026-04-05&end_date=2026-11-01$")
})

test_that("update_plan starts a window at its package's refetch floor when that is earlier", {
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c"),
                      last_day = c("2026-09-20", "2026-09-20", "2025-06-01"))
  p0 <- update_plan(roster, "2026-10-01", list(), as.Date("2026-11-01"))
  expect_identical(p0$start, rep(as.Date("2026-09-01"), 3))
  expect_identical(p0$dormant, 3L)
  p <- update_plan(roster, "2026-10-01", list(), as.Date("2026-11-01"),
                   floor_all = as.Date("2026-08-02"), floors = list(b = "2026-07-01"))
  expect_identical(p$start, as.Date(c("2026-08-02", "2026-07-01", "2026-08-02")))
  expect_identical(p$w, 1:2)
  # a floor later than the usual start changes nothing
  p1 <- update_plan(roster, "2026-10-01", list(), as.Date("2026-11-01"),
                    floor_all = as.Date("2026-09-20"))
  expect_identical(p1$start, p0$start)
  # a floor further back than the activity window keeps every release with a
  # stored day after it in the window, so the overlap holds only window days
  p2 <- update_plan(roster, "2026-10-01", list(), as.Date("2026-11-01"),
                    floor_all = as.Date("2025-05-01"))
  expect_identical(p2$w, 1:3)
})

test_that("next_floors sets a floor only for new days the next window would not reach", {
  edge <- stats::setNames(as.Date(c("2026-07-01", "2026-07-01", "2026-06-01")), c("a", "b", "c"))
  none <- stats::setNames(rep(as.Date(NA), 3), names(edge))
  empty <- stats::setNames(list(), character(0))
  nf <- function(t_eff, refreshed = c("a", "b"), floor = none, active = names(edge))
    next_floors(edge, floor, refreshed, active, as.Date("2026-07-01"), as.Date(t_eff))
  # a month of 31 days: the next window starts on the first new day
  expect_identical(nf("2026-08-01"), list(all = NULL, packages = empty))
  # one day more, and the first new day would be fetched only once
  expect_identical(nf("2026-08-02"), list(all = "2026-07-02", packages = empty))
  # a package refreshed from an earlier edge gets its own, earlier floor
  expect_identical(nf("2026-08-01", refreshed = c("a", "b", "c")),
                   list(all = NULL, packages = list(c = "2026-06-02")))
  # a floor a package could not use is kept for it, and only for it
  fl <- none; fl[] <- as.Date("2026-06-10")
  expect_identical(nf("2026-08-01", refreshed = "a", floor = fl),
                   list(all = NULL, packages = list(b = "2026-06-10", c = "2026-06-10")))
  expect_identical(nf("2026-08-01", refreshed = "a", floor = fl, active = c("a", "b")),
                   list(all = NULL, packages = list(b = "2026-06-10")))
  # a kept floor later than the new common one is covered by it
  fl[] <- as.Date("2026-07-20")
  expect_identical(nf("2026-08-20", refreshed = "a", floor = fl, active = c("a", "b")),
                   list(all = "2026-07-02", packages = empty))
  # a package the published release held keeps the floor it is refreshed
  # from one more run, when that reaches back past its revision window
  held <- function(floor) next_floors(edge, floor, c("a", "b"), c("a", "b"), as.Date("2026-07-01"),
                                      as.Date("2026-08-01"), held_before = "b")
  fl[] <- as.Date("2026-05-01")
  expect_identical(held(fl), list(all = NULL, packages = list(b = "2026-05-01")))
  fl[] <- as.Date("2026-06-01")
  expect_identical(held(fl), list(all = NULL, packages = empty))
})

# A world counted through 2026-08-01 where every release has a download before
# that edge: zoo v1 every day from June, the others now and then. `late` is
# zoo v1's count on 2026-08-10 before Launchpad revised it up to 2.
long_world <- function(n_fill = 0L) {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-zoo", "r-cran-abc"), version = c("1", "2", "1"))
  lp <- rbind(
    lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-12-31"), by = "day")), 2),
    lp_rows(2, c("2026-07-15", "2026-08-15", "2026-09-15"), c(1, 1, 6)),
    lp_rows(3, c("2026-07-20", "2026-08-20", "2026-09-20"), c(1, 3, 8)))
  if (n_fill > 0L) {
    fid <- 100L + seq_len(n_fill)
    roster <- rbind(roster, mk_roster(sprintf("r-cran-fill%02d", seq_len(n_fill)), pub_id = fid))
    lp <- rbind(lp, lp_rows(fid, "2026-07-10", 1), lp_rows(fid, "2026-08-10", 3),
                lp_rows(fid, "2026-09-10", 1))
  }
  early <- lp
  early$count[early$pub_id == 1L & early$day == "2026-08-10"] <- 1L
  list(roster = roster, lp = lp, early = early)
}

test_that("days first counted in a run longer than the revision window are fetched again next run", {
  # nothing was published in September, so the October run counts 61 new days,
  # and a window starting 30 days before its edge would never see 2026-08-10
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- long_world()
  seed_world(pub, w$roster, w$lp, "2026-08-01")
  res <- run_q(lp_io(pub, w$roster, w$early), out)          # T = 2026-10-01
  m <- res$manifest
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(m$refetch_from, "2026-08-02")
  expect_length(m$package_refetch_from, 0L)
  expect_true(any(grepl("the next run re-fetches .* from 2026-08-02", res$messages)))
  expect_identical(cnt(history_of(out), "zoo", "2026-08-10"), 1L)
  publish(out, pub)
  io2 <- lp_io(pub, w$roster, w$lp, now = t_at("2026-11-03"))
  res2 <- run_q(io2, out2)
  expect_match(requested(io2), "start_date=2026-08-02&end_date=2026-11-01$")
  expect_identical(history_of(out2), truth_daily(w$lp, w$roster, "2026-11-01"))
  # a month-long run needs no floor: the next window covers it
  expect_null(res2$manifest$refetch_from)
  expect_length(res2$manifest$package_refetch_from, 0L)
  expect_silent(verify_release(out2, read_manifest(out2)))
})

test_that("a package held after a long run keeps its refetch floor until the run after its refresh", {
  pub <- withr::local_tempdir()
  w <- long_world(n_fill = 50L)                     # 53 window releases: one may be held
  seed_world(pub, w$roster, w$lp, "2026-08-01")
  step <- function(day, lp, gone = function(u) FALSE) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    io <- lp_io(pub, w$roster, lp, now = t_at(day), pool_fail = gone, serial_fail = gone)
    res <- run_q(io, out)
    publish(out, pub)
    list(m = res$manifest, io = io, out = out)
  }
  expect_identical(step("2026-10-03", w$early)$m$refetch_from, "2026-08-02")
  # November: zoo v2 fails, so zoo is held and has not had its second look
  r2 <- step("2026-11-03", w$lp, gone = function(u) url_pub(u) == 2L)
  expect_identical(r2$m$package_edges, list(zoo = "2026-10-01"))
  expect_null(r2$m$refetch_from)
  expect_identical(r2$m$package_refetch_from, list(zoo = "2026-08-02"))
  # December: zoo is refreshed from its own floor, the others from 30 days back
  r3 <- step("2026-12-03", w$lp)
  u <- requested(r3$io)
  expect_match(u[url_pub(u) %in% 1:2], "start_date=2026-08-02&end_date=2026-12-01$")
  expect_match(u[!url_pub(u) %in% 1:2], "start_date=2026-10-02&end_date=2026-12-01$")
  expect_identical(history_of(r3$out), truth_daily(w$lp, w$roster, "2026-12-01"))
  # a refresh after a hold keeps the floor it used one more run, which also
  # covers the two months zoo caught up at once
  expect_identical(r3$m$package_refetch_from, list(zoo = "2026-08-02"))
  expect_null(r3$m$refetch_from)
  expect_length(r3$m$package_edges, 0L)
  r4 <- step("2027-01-03", w$lp)
  u <- requested(r4$io)
  expect_match(u[url_pub(u) %in% 1:2], "start_date=2026-08-02&end_date=2027-01-01$")
  expect_length(r4$m$package_refetch_from, 0L)
})

# --- window replacement and full-history additions ------------------------------

test_that("the window replaces stored days from S, keeps history before it, and stops at T", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  live <- w$lp   # a late upward revision inside the window
  live$count[live$pub_id == 1L & live$day == "2026-08-20"] <- 5L
  io <- lp_io(pub, w$roster, live)
  res <- run_q(io, out)
  expect_true(res$publish)
  h <- history_of(out)
  expect_identical(h, truth_daily(live, w$roster, "2026-10-01"))
  expect_identical(cnt(h, "zoo", "2026-08-20"), 5L)      # replaced, not 2 + 5
  expect_identical(cnt(h, "zoo", "2026-08-15"), 3L)      # both releases summed
  expect_identical(cnt(h, "zoo", "2026-01-10"), 100L)    # before S: untouched
  expect_false(any(h$date > "2026-10-01"))
  expect_match(requested(io), "start_date=2026-08-02&end_date=2026-10-01$")

  m <- res$manifest
  expect_identical(m$history_method, "summed")
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(m$summary$latest_date, "2026-10-01")
  expect_length(m$package_edges, 0L)
  expect_identical(m$unfetched_releases, 0L)
  expect_true(m$complete)
  expect_identical(m$source_kind, "launchpad")
  expect_setequal(res$changed_shards, c("c2d4u-downloads-2026.db", "c2d4u-downloads-recent.db",
                                        "c2d4u-downloads-summary.db"))
  expect_identical(readLines(file.path(out, UPLOAD_LIST)), res$changed_shards)
  expect_identical(read_manifest(out)$counted_through, "2026-10-01")
  expect_silent(verify_release(out, read_manifest(out)))
  # the summary and the recent shard are anchored on counted_through
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$last_date[s$package == "zoo"], "2026-10-01")
  expect_identical(s$total_30d[s$package == "abc"], 8L)
})

test_that("a never-fetched release adds its history before S and joins the window after it", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster(c("r-cran-zoo", "r-cran-abc"), version = c("3", "2"),
                                      pub_id = 4:5, done = 0L))
  lp <- rbind(w$lp, lp_rows(1, "2019-03-01", 100),
              lp_rows(4, c("2019-03-01", "2026-09-10", "2026-10-02"), c(5, 2, 1)))
  seed_world(pub, roster, lp, E_STD)
  expect_identical(cnt(history_of(pub), "zoo", "2019-03-01"), 100L)
  io <- lp_io(pub, roster, lp)
  res <- run_q(io, out)
  h <- history_of(out)
  expect_identical(cnt(h, "zoo", "2019-03-01"), 105L)   # 100 stored + 5 new
  expect_identical(cnt(h, "zoo", "2026-09-10"), 4L)     # v1 2 + v3 2, inside the window
  expect_identical(h, truth_daily(lp, roster, "2026-10-01"))
  # fetched in full with no end_date; the day after T is not counted but moves last_day
  f_urls <- requested(io)[url_pub(requested(io)) %in% 4:5]
  expect_length(f_urls, 2L)
  expect_false(any(grepl("start_date|end_date", f_urls)))
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 4L], 1L)
  expect_identical(rel$last_day[rel$pub_id == 4L], "2026-10-02")
  expect_identical(rel$done[rel$pub_id == 5L], 2L)      # answered empty once: asked again
  expect_true(is.na(rel$last_day[rel$pub_id == 5L]))
  expect_identical(res$manifest$summary$full_history_releases, 2L)
  expect_identical(res$manifest$unfetched_releases, 0L)
  expect_identical(res$manifest$empty_once_releases, 1L)
  expect_setequal(res$changed_shards, c("c2d4u-downloads-2019.db", "c2d4u-downloads-2026.db",
                                        "c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db"))
})

test_that("a held package gets a never-fetched release's days through its edge, the rest next run", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)
  roster <- rbind(w$roster, mk_roster("r-cran-zoo", version = "3", pub_id = 4L, done = 0L))
  # v3 has a day before S_p, one inside zoo's overlap and one after its edge
  lp <- rbind(w$lp, lp_rows(4, c("2019-03-01", "2026-08-20", "2026-09-20"), c(5, 7, 4)))
  seed_world(pub, roster, lp, E_STD)
  gone <- function(u) url_pub(u) == 2L
  res <- run_q(lp_io(pub, roster, lp, pool_fail = gone, serial_fail = gone), out)
  expect_identical(res$manifest$package_edges, list(zoo = "2026-09-01"))
  # zoo's data stops at its edge, and through it every release is counted
  expect_identical(history_of(out, "zoo"), truth_daily(lp, roster, E_STD, pubs = c(1L, 2L, 4L)))
  expect_identical(cnt(history_of(out), "zoo", "2026-08-20"), 2L + 7L)
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 4L], 1L)
  expect_identical(rel$last_day[rel$pub_id == 4L], "2026-09-20")

  publish(out, pub)
  res2 <- run_q(lp_io(pub, roster, lp, now = t_at("2026-11-03")), out2)
  expect_length(res2$manifest$package_edges, 0L)
  expect_identical(history_of(out2), truth_daily(lp, roster, "2026-11-01"))
})

test_that("update_plan fetches empty-once releases in full, after the never-fetched ones", {
  roster <- mk_roster(c("r-cran-a", "r-cran-b", "r-cran-c", "r-cran-d"),
                      last_day = c("2026-05-01", NA, NA, NA), done = c(1L, 2L, 0L, 1L))
  p <- update_plan(roster, "2026-05-05", list(), as.Date("2026-10-01"))
  expect_identical(p$w, 1L)
  expect_identical(p$f, c(3L, 2L))
  expect_identical(p$never, 4L)
  expect_length(p$dormant, 0L)
})

test_that("a release answering one empty page is asked again, and a second empty answer settles it", {
  pub <- withr::local_tempdir()
  w <- std_world(through = "2026-12-31")
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 5L, done = 0L))
  seed_world(pub, roster, w$lp, E_STD)
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    io <- lp_io(pub, roster, w$lp, now = t_at(day))
    res <- run_q(io, out)
    publish(out, pub)
    list(res = res, io = io, rel = load_releases(file.path(out, "c2d4u-downloads-recent.db")))
  }
  r1 <- step("2026-10-03")
  expect_identical(r1$rel$done[r1$rel$pub_id == 5L], 2L)
  expect_true(is.na(r1$rel$last_day[r1$rel$pub_id == 5L]))
  m1 <- r1$res$manifest
  expect_identical(m1$unfetched_releases, 0L)
  expect_identical(m1$empty_once_releases, 1L)
  expect_identical(m1$summary$empty_once_releases, 1L)
  expect_false(m1$complete)
  # the next month asks for its whole history again
  r2 <- step("2026-11-03")
  u <- requested(r2$io)
  expect_identical(u[url_pub(u) == 5L], counts_urls(roster[roster$pub_id == 5L, ]))
  expect_true(any(grepl("1 full-history releases \\(1 of them answered empty once before\\)",
                        r2$res$messages)))
  expect_identical(r2$rel$done[r2$rel$pub_id == 5L], 1L)
  expect_true(is.na(r2$rel$last_day[r2$rel$pub_id == 5L]))
  m2 <- r2$res$manifest
  expect_identical(m2$summary$empty_once_rechecked, 1L)
  expect_identical(m2$empty_once_releases, 0L)
  expect_true(m2$complete)
  # settled as never downloaded: not fetched again
  r3 <- step("2026-12-03")
  expect_false(5L %in% url_pub(requested(r3$io)))
  expect_identical(r3$res$manifest$summary$excluded_never_downloaded, 1L)
})

test_that("a release that answered empty once but has downloads is counted in full when asked again", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-zoo", version = "3", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, c("2019-03-01", "2026-08-20", "2026-09-25"), c(5, 7, 4)))
  seed_world(pub, roster, lp, E_STD)
  # zoo v3's first full answer is a transient empty page
  io <- lp_io(pub, roster, lp)
  many <- io$fetch_many
  io$fetch_many <- function(urls, deadline = Inf) {
    b <- many(urls, deadline)
    for (k in which(url_pub(urls) == 4L))
      b[[k]] <- counts_json("r-cran-zoo", "3", character(0), integer(0))
    b
  }
  run_q(io, out)
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 4L], 2L)
  publish(out, pub)
  run_q(lp_io(pub, roster, lp, now = t_at("2026-11-03")), out2)
  expect_identical(history_of(out2), truth_daily(lp, roster, "2026-11-01"))
  rel <- load_releases(file.path(out2, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 4L], 1L)
  expect_identical(rel$last_day[rel$pub_id == 4L], "2026-09-25")
})

test_that("a release whose later page answers empty is fetched again, not settled short", {
  # big has 944 days, four pages; page 2 of its full history (ws.start=300)
  # answers empty with no next link, as a transient empty 200 would
  pub <- withr::local_tempdir()
  w <- std_world(through = "2027-01-02")
  roster <- rbind(w$roster, mk_roster("r-cran-big", pub_id = 7L, done = 0L))
  lp <- rbind(w$lp, lp_rows(7, format(seq(as.Date("2024-06-01"), as.Date("2026-12-31"), by = "day")), 1))
  seed_world(pub, roster, lp, E_STD)
  io <- lp_io(pub, roster, lp)
  bad <- function(u) url_pub(u) == 7L && grepl("ws.start=300", u, fixed = TRUE)
  empty <- counts_json("r-cran-big", "1.0", character(0), integer(0), total_size = 944, start = 300)
  one <- io$fetch; many <- io$fetch_many
  io$fetch <- function(u) if (bad(u)) empty else one(u)
  io$fetch_many <- function(urls, deadline = Inf) {
    b <- many(urls, deadline)
    b[vapply(urls, bad, logical(1))] <- list(empty)
    b
  }
  out <- withr::local_tempdir()
  res <- run_q(io, out)
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 7L], 0L)
  expect_identical(nrow(history_of(out, "big")), 0L)
  expect_identical(res$manifest$unfetched_releases, 1L)
  expect_false(res$manifest$complete)
  publish(out, pub)
  out2 <- withr::local_tempdir()
  res2 <- run_q(lp_io(pub, roster, lp, now = t_at("2026-11-03")), out2)
  expect_identical(history_of(out2), truth_daily(lp, roster, "2026-11-01"))
  expect_true(res2$manifest$complete)
})

test_that("a held package's never-fetched days inside its overlap count toward its own edge", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)
  roster <- rbind(w$roster, mk_roster("r-cran-zoo", version = "9", pub_id = 9L, done = 0L))
  lp <- rbind(w$lp, lp_rows(9, c("2026-07-30", "2026-08-05", "2026-08-30"), c(12, 13, 14)))
  seed_world(pub, roster, lp, E_STD)
  gone <- function(u) url_pub(u) == 2L
  run_q(lp_io(pub, roster, lp, pool_fail = gone, serial_fail = gone), out)
  expect_identical(history_of(out, "zoo"), truth_daily(lp, roster, E_STD, pubs = c(1L, 2L, 9L)))
})


test_that("a package with a failed window release is held at its edge and caught up next run", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)
  seed_world(pub, w$roster, w$lp, E_STD)
  before <- history_of(pub, "zoo")
  gone <- function(u) url_pub(u) == 2L
  io <- lp_io(pub, w$roster, w$lp, pool_fail = gone, serial_fail = gone)
  res <- run_q(io, out)
  m <- res$manifest
  expect_identical(m$package_edges, list(zoo = "2026-09-01"))
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(m$summary$held_packages, 1L)
  expect_false(m$complete)
  expect_identical(history_of(out, "zoo"), before)     # held: stored days untouched
  others <- setdiff(w$roster$pub_id, 1:2)
  expect_identical(history_of(out, not = "zoo"), truth_daily(w$lp, w$roster, "2026-10-01", others))

  # a month later the release answers again: zoo is fetched from its own S_p
  publish(out, pub)
  io2 <- lp_io(pub, w$roster, w$lp, now = t_at("2026-11-03"))
  res2 <- run_q(io2, out2)
  u <- requested(io2)
  expect_match(u[url_pub(u) %in% 1:2], "start_date=2026-08-02&end_date=2026-11-01$")
  expect_match(u[!url_pub(u) %in% 1:2], "start_date=2026-09-01&end_date=2026-11-01$")
  expect_length(res2$manifest$package_edges, 0L)
  expect_true(res2$manifest$complete)
  expect_identical(history_of(out2), truth_daily(w$lp, w$roster, "2026-11-01"))
})

test_that("a held package's summary windows end at its own edge, not at counted_through", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)
  seed_world(pub, w$roster, w$lp, E_STD)
  gone <- function(u) url_pub(u) == 2L
  run_q(lp_io(pub, w$roster, w$lp, pool_fail = gone, serial_fail = gone), out)
  prev <- load_summary(file.path(pub, "c2d4u-downloads-summary.db"))
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  cols <- c("total_30d", "total_90d", "total_365d", "avg_daily_30d", "trend", "cnt_total", "last_date")
  # zoo's stored days and its edge did not move, so neither do its windows
  expect_identical(as.list(s[s$package == "zoo", cols]), as.list(prev[prev$package == "zoo", cols]))
  expect_identical(s$total_30d[s$package == "zoo"], 2L * 31L + 1L)
  expect_identical(s$rank_30d[s$package == "zoo"], 1L)
  # a refreshed package is anchored on counted_through
  expect_identical(s$total_30d[s$package == "abc"], 8L)
  expect_identical(s, load_summary(file.path(out, "c2d4u-downloads-recent.db")))
})

test_that("window failures above the outage fraction stop the run instead of holding", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)     # 53 window releases: 1 failure holds, 2 stop
  seed_world(pub, w$roster, w$lp, E_STD)
  gone <- function(u) url_pub(u) %in% c(2L, 3L)
  io <- lp_io(pub, w$roster, w$lp, pool_fail = gone, serial_fail = gone)
  expect_error(run_q(io, out), "outage: 2 of 53 window releases failed")
  expect_nothing_written(out, pub)
})

test_that("a package whose re-fetched overlap dropped is held; more than the limit stops", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-big", pub_id = 9L))
  stored <- rbind(w$lp, lp_rows(9, c("2026-08-15", "2026-09-15"), c(100, 50)))
  live <- stored
  live$count[live$pub_id == 9L & live$day == "2026-08-15"] <- 60L
  seed_world(pub, roster, stored, E_STD)
  before <- history_of(pub, "big")
  res <- run_q(lp_io(pub, roster, live), out)
  expect_identical(res$manifest$package_edges, list(big = "2026-09-01"))
  expect_identical(history_of(out, "big"), before)
  expect_identical(history_of(out, not = "big"), truth_daily(live, roster, "2026-10-01", 1:3))
  expect_true(any(grepl("regressed.*big", res$messages)))

  # eleven regressed packages is a source regression, not a few bad records
  pub2 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  ids <- 20L + 1:11
  roster2 <- rbind(w$roster, mk_roster(sprintf("r-cran-big%02d", 1:11), pub_id = ids))
  stored2 <- rbind(w$lp, lp_rows(ids, "2026-08-15", 100))
  live2 <- stored2
  live2$count[live2$pub_id %in% ids] <- 60L
  seed_world(pub2, roster2, stored2, E_STD)
  expect_error(run_q(lp_io(pub2, roster2, live2), out2), "source regression: 11 packages")
  expect_nothing_written(out2, pub2)
})

test_that("a stored day re-fetched lower keeps its stored count, so a short answer loses nothing", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- std_world(n_fill = 60L)
  seed_world(pub, w$roster, w$lp, E_STD)
  # abc's only release answers one empty page: its 3 stored downloads in the
  # overlap are too few for the per-package guard and too few to move the total
  io <- lp_io(pub, w$roster, w$lp)
  many <- io$fetch_many
  io$fetch_many <- function(urls, deadline = Inf) {
    b <- many(urls, deadline)
    for (k in which(url_pub(urls) == 3L)) b[[k]] <- counts_json("r-cran-abc", "1", character(0), integer(0))
    b
  }
  res <- run_q(io, out)
  expect_length(res$manifest$package_edges, 0L)
  expect_identical(cnt(history_of(out), "abc", "2026-08-20"), 3L)
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$cnt_total[s$package == "abc"], 3L)
  # the next run no longer re-fetches that day, and the history is whole
  publish(out, pub)
  run_q(lp_io(pub, w$roster, w$lp, now = t_at("2026-11-03")), out2)
  expect_identical(history_of(out2), truth_daily(w$lp, w$roster, "2026-11-01"))
})

test_that("an overlap re-fetched within the drop tolerance is refreshed, keeping the higher days", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-big", pub_id = 9L))
  stored <- rbind(w$lp, lp_rows(9, c("2026-08-15", "2026-09-15"), c(100, 5)))
  live <- stored
  live$count[live$pub_id == 9L & live$day == "2026-08-15"] <- 99L    # 1% lower
  live <- rbind(live, lp_rows(9, "2026-09-25", 4))
  seed_world(pub, roster, stored, E_STD)
  res <- run_q(lp_io(pub, roster, live), out)
  expect_length(res$manifest$package_edges, 0L)
  h <- history_of(out)
  expect_identical(cnt(h, "big", "2026-08-15"), 100L)
  expect_identical(cnt(h, "big", "2026-09-25"), 4L)                  # refreshed past its edge
  expect_identical(history_of(out, not = "big"), truth_daily(live, roster, "2026-10-01", 1:3))
})

test_that("a stored day Launchpad no longer returns stays in every published shard", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-small"))
  stored <- rbind(lp_rows(1, format(seq(as.Date("2026-01-01"), as.Date("2026-03-01"), by = "day")), 50),
                  lp_rows(2, "2025-12-20", 3))
  live <- stored[stored$pub_id == 1L, ]                 # small's 2025-12-20 is gone
  seed_world(pub, roster, stored, "2026-01-10")        # S = 2025-12-11
  res <- run_q(lp_io(pub, roster, live, now = t_at("2026-02-03")), out)
  expect_true(res$publish)
  # the year shard, the recent shard and the summary agree on it
  expect_identical(cnt(history_of(out), "small", "2025-12-20"), 3L)
  expect_identical(cnt(load_daily(file.path(out, "c2d4u-downloads-recent.db")), "small", "2025-12-20"), 3L)
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$cnt_total[s$package == "small"], 3L)
  expect_true("c2d4u-downloads-2025.db" %in% res$changed_shards)
})

test_that("a drop spread over packages too small to check alone stops the run", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  ids <- 30L + 1:30
  roster <- rbind(w$roster, mk_roster(sprintf("r-cran-sm%02d", 1:30), pub_id = ids))
  stored <- rbind(w$lp, lp_rows(ids, "2026-08-15", 10))
  live <- stored
  live$count[live$pub_id %in% ids] <- 9L    # 10% each, all below REGRESSION_MIN_STORED
  seed_world(pub, roster, stored, E_STD)
  expect_error(run_q(lp_io(pub, roster, live), out), "source regression: the window overlap")
  expect_nothing_written(out, pub)
})

# --- source gaps -------------------------------------------------------------------

test_that("window_gaps caps at a trailing zero run and records interior ones", {
  tot <- c(`2026-09-02` = 5, `2026-09-03` = 1, `2026-09-08` = 2)
  g <- window_gaps(tot, "2026-09-01", "2026-09-12", known = list())
  expect_identical(g$t_eff, as.Date("2026-09-08"))
  expect_identical(g$detected, list(list(from = "2026-09-04", to = "2026-09-07")))
  expect_identical(g$trailing, list(from = "2026-09-09", to = "2026-09-12"))
  # runs shorter than ZERO_RUN_DAYS are ordinary quiet days
  g2 <- window_gaps(c(`2026-09-02` = 1, `2026-09-05` = 1, `2026-09-08` = 1, `2026-09-10` = 1),
                    "2026-09-01", "2026-09-12", known = list())
  expect_identical(g2$t_eff, as.Date("2026-09-12"))
  expect_length(g2$detected, 0L)
  expect_null(g2$trailing)
  # nothing reported at all holds counted_through where it was
  g3 <- window_gaps(numeric(0), "2026-09-01", "2026-09-12", known = list())
  expect_identical(g3$t_eff, as.Date("2026-09-01"))
  # an empty window
  g4 <- window_gaps(numeric(0), "2026-09-01", "2026-09-01", known = list())
  expect_identical(g4$t_eff, as.Date("2026-09-01"))
  expect_null(g4$trailing)
})

test_that("window_gaps neither flags nor caps on days inside a known source gap", {
  known <- list(list(from = "2026-09-04", to = "2026-09-07"))
  g <- window_gaps(c(`2026-09-02` = 5, `2026-09-03` = 1, `2026-09-08` = 2),
                   "2026-09-01", "2026-09-08", known = known)
  expect_length(g$detected, 0L)
  expect_identical(g$t_eff, as.Date("2026-09-08"))
  g2 <- window_gaps(c(`2026-09-02` = 5, `2026-09-03` = 1), "2026-09-01", "2026-09-07", known = known)
  expect_identical(g2$t_eff, as.Date("2026-09-07"))
  expect_null(g2$trailing)
  # zero days after the known gap still count
  g3 <- window_gaps(c(`2026-09-02` = 5, `2026-09-03` = 1), "2026-09-01", "2026-09-10", known = known)
  expect_identical(g3$t_eff, as.Date("2026-09-07"))
  expect_identical(g3$trailing, list(from = "2026-09-08", to = "2026-09-10"))
})

test_that("window_gaps counts a quiet stretch longer than STALL_MAX_DAYS as downloads that stopped", {
  # the whole window is quiet, and so were the days since the last download before it
  g <- window_gaps(numeric(0), "2026-12-01", "2027-01-01", known = list(), last_seen = "2026-09-20")
  expect_identical(g$t_eff, as.Date("2027-01-01"))
  expect_null(g$trailing)
  expect_identical(g$stopped, list(from = "2026-09-21", to = "2027-01-01"))
  # it is recorded from its first quiet day, before the window, so the days
  # already counted as quiet are fetched again with it
  expect_identical(g$detected, list(list(from = "2026-09-21", to = "2027-01-01", kind = "stopped")))
  # a quiet stretch no longer than the bound is still a stall
  g2 <- window_gaps(numeric(0), "2026-10-01", "2026-11-01", known = list(), last_seen = "2026-09-20")
  expect_identical(g2$t_eff, as.Date("2026-10-01"))
  expect_identical(g2$trailing, list(from = "2026-10-02", to = "2026-11-01"))
  expect_null(g2$stopped)
  # days inside a known gap do not count toward the bound
  g3 <- window_gaps(numeric(0), "2026-12-01", "2027-01-01",
                    known = list(list(from = "2026-09-21", to = "2026-11-30")), last_seen = "2026-09-20")
  expect_identical(g3$t_eff, as.Date("2026-12-01"))
  expect_null(g3$stopped)
  # a stretch that began inside the window can pass the bound too
  g4 <- window_gaps(c(`2026-06-02` = 3), "2026-06-01", "2026-10-01", known = list())
  expect_identical(g4$t_eff, as.Date("2026-10-01"))
  expect_identical(g4$stopped, list(from = "2026-06-03", to = "2026-10-01"))
  expect_identical(g4$detected, list(list(from = "2026-06-03", to = "2026-10-01", kind = "stopped")))
})

test_that("add_gaps marks an entry extended by a stopped stretch as stopped", {
  old <- list(list(from = "2026-02-01", to = "2026-02-05"),
              list(from = "2026-09-10", to = "2026-10-01", kind = "stopped"))
  # a later run of the same stretch extends it and it stays stopped
  expect_identical(add_gaps(old, list(list(from = "2026-10-02", to = "2026-11-01", kind = "stopped"))),
                   list(old[[1]], list(from = "2026-09-10", to = "2026-11-01", kind = "stopped")))
  # an interior run right after it (downloads came back) is no longer stopped
  expect_identical(add_gaps(old, list(list(from = "2026-10-02", to = "2026-10-06"))),
                   list(old[[1]], list(from = "2026-09-10", to = "2026-10-06")))
  # a stopped part right after an interior gap extends that gap as stopped
  stopped <- list(list(from = "2026-02-06", to = "2026-06-01", kind = "stopped"))
  expect_identical(add_gaps(old[1], stopped),
                   list(list(from = "2026-02-01", to = "2026-06-01", kind = "stopped")))
})

test_that("downloads that stop for good are counted as stopped, and the archive then goes dormant", {
  pub <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-09-20"), by = "day")), 1),
              lp_rows(2, "2026-08-10", 3))
  seed_world(pub, roster, lp, E_STD)
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    res <- run_q(lp_io(pub, roster, lp, now = t_at(day)), out)
    if (isTRUE(res$publish)) publish(out, pub)
    res
  }
  r1 <- step("2026-10-03")                              # 11 quiet days: a stall
  expect_identical(r1$manifest$counted_through, "2026-09-20")
  expect_false(step("2026-11-03")$publish)              # 42 quiet days: still a stall
  r3 <- step("2027-01-03")                              # 103 quiet days: downloads stopped
  expect_true(r3$publish)
  expect_identical(r3$manifest$counted_through, "2027-01-01")
  expect_true(any(grepl("^::warning::.*no downloads from 2026-09-21 to 2027-01-01", r3$stdout)))
  expect_identical(r3$manifest$detected_gaps,
                   list(list(from = "2026-09-21", to = "2027-01-01", kind = "stopped")))
  expect_identical(history_of(pub), truth_daily(lp, roster, "2027-01-01"))
  r4 <- step("2027-02-03")                              # the stretch goes on: one record
  expect_identical(r4$manifest$counted_through, "2027-02-01")
  expect_identical(r4$manifest$detected_gaps,
                   list(list(from = "2026-09-21", to = "2027-02-01", kind = "stopped")))
  expect_identical(step("2027-10-03")$manifest$counted_through, "2027-10-01")
  # the last download is now more than a year before the edge: every release
  # is dormant, and a quiet probe publishes a heartbeat
  r6 <- step("2027-11-03")
  expect_true(r6$publish)
  expect_identical(r6$manifest$source_kind, "dormant")
  expect_identical(r6$manifest$counted_through, "2027-10-01")
})

# The world of the test above: zoo downloaded every day until 2026-09-20 and
# abc once, published through 2026-09-01, then stepped month by month. `lp`
# is what Launchpad answers from `from` on (a later answer replaces it), and
# `gone` fails a request in the pool and serially. n_fill releases downloaded
# once on 2026-08-12 let one window release fail without an outage. The
# published release lives as long as `envir`.
stop_world <- function(n_fill = 0L, envir = parent.frame()) {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-09-20"), by = "day")), 1),
              lp_rows(2, "2026-08-10", 3))
  if (n_fill > 0L) {
    fid <- 100L + seq_len(n_fill)
    roster <- rbind(roster, mk_roster(sprintf("r-cran-fill%02d", seq_len(n_fill)), pub_id = fid))
    lp <- rbind(lp, lp_rows(fid, "2026-08-12", 1))
  }
  pub <- withr::local_tempdir(.local_envir = envir)
  seed_world(pub, roster, lp, E_STD)
  env <- environment()
  gone <- function(u) FALSE
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    io <- lp_io(pub, roster, env$lp, now = t_at(day), pool_fail = env$gone, serial_fail = env$gone)
    res <- run_q(io, out)
    if (isTRUE(res$publish)) publish(out, pub)
    res$io <- io; res$out <- out
    res
  }
  list(roster = roster, env = env, pub = pub, step = step)
}

test_that("a stopped stretch is fetched again every month while it is counted as stopped", {
  w <- stop_world()
  w$step("2026-10-03"); w$step("2026-11-03")
  r0 <- w$step("2027-01-03")
  expect_identical(r0$manifest$refetch_from, "2026-09-21")
  for (day in c("2027-02-03", "2027-03-03")) {
    r <- w$step(day)
    expect_match(requested(r$io), "start_date=2026-09-21&", info = day)
    expect_identical(r$manifest$refetch_from, "2026-09-21", info = day)
    expect_identical(tail(r$manifest$detected_gaps, 1)[[1]]$kind, "stopped", info = day)
  }
})

test_that("a stopped stretch Launchpad fills in later is counted, and its gap trimmed", {
  w <- stop_world()
  for (day in c("2026-10-03", "2026-11-03", "2027-01-03", "2027-02-03")) w$step(day)
  # after February's run, Launchpad fills in six days of the stretch
  w$env$lp <- rbind(w$env$lp, lp_rows(1, format(seq(as.Date("2026-10-05"), as.Date("2026-10-10"),
                                                    by = "day")), 4))
  r <- w$step("2027-03-03")
  expect_true(r$publish)
  expect_match(requested(r$io), "start_date=2026-09-21&end_date=2027-03-01$")
  expect_identical(history_of(w$pub), truth_daily(w$env$lp, w$roster, "2027-03-01"))
  m <- r$manifest
  expect_identical(m$counted_through, "2027-03-01")
  expect_identical(m$detected_gaps, list(list(from = "2026-09-21", to = "2026-10-04"),
                                         list(from = "2026-10-11", to = "2027-03-01", kind = "stopped")))
  # the filled days were counted for the first time: the next run fetches
  # them again, and only then does the floor move to where the rest begins
  expect_identical(m$refetch_from, "2026-09-21")
  r1 <- w$step("2027-04-03")
  expect_match(requested(r1$io), "start_date=2026-09-21&")
  expect_identical(r1$manifest$refetch_from, "2026-10-11")
  # downloads come back: the stretch is an ordinary gap and the floor goes
  w$env$lp <- rbind(w$env$lp, lp_rows(1, format(seq(as.Date("2027-04-10"), as.Date("2027-05-10"),
                                                    by = "day")), 2))
  r2 <- w$step("2027-05-03")
  expect_identical(r2$manifest$counted_through, "2027-05-01")
  expect_identical(tail(r2$manifest$detected_gaps, 1),
                   list(list(from = "2026-10-11", to = "2027-04-09")))
  expect_null(r2$manifest$refetch_from)
  expect_identical(history_of(w$pub), truth_daily(w$env$lp, w$roster, "2027-05-01"))
})

test_that("days filled in inside a stopped stretch are fetched in two runs, so a revision counts", {
  w <- stop_world()
  for (day in c("2026-10-03", "2026-11-03", "2027-01-03", "2027-02-03")) w$step(day)
  fill <- format(seq(as.Date("2026-10-05"), as.Date("2026-10-10"), by = "day"))
  w$env$lp <- rbind(w$env$lp, lp_rows(1, fill, 2))              # a first, partial count
  w$step("2027-03-03")
  expect_identical(cnt(history_of(w$pub), "zoo", "2026-10-07"), 2L)
  lp <- w$env$lp                                                 # then revised up
  lp$count[lp$pub_id == 1L & lp$day %in% fill] <- 4L
  w$env$lp <- lp
  w$step("2027-04-03")
  expect_identical(cnt(history_of(w$pub), "zoo", "2026-10-07"), 4L)
  expect_identical(history_of(w$pub), truth_daily(w$env$lp, w$roster, "2027-04-01"))
  # downloads come back with the fill: the old first day is still fetched once more
  w2 <- stop_world()
  for (day in c("2026-10-03", "2026-11-03", "2027-01-03", "2027-02-03")) w2$step(day)
  w2$env$lp <- rbind(w2$env$lp, lp_rows(1, fill, 2),
                     lp_rows(1, format(seq(as.Date("2027-02-10"), as.Date("2027-05-01"), by = "day")), 1))
  r <- w2$step("2027-03-03")
  expect_identical(tail(r$manifest$detected_gaps, 1), list(list(from = "2026-10-11", to = "2027-02-09")))
  expect_identical(r$manifest$refetch_from, "2026-09-21")
  r2 <- w2$step("2027-04-03")
  expect_match(requested(r2$io), "start_date=2026-09-21&")
  expect_null(r2$manifest$refetch_from)
})

# stop_world with 60 filler releases (62 window releases, so one may be held),
# stepped until the stretch from 2026-09-21 is counted as stopped. Launchpad
# then fills in FILL for zoo and abc (and adds `back`), and abc's release
# fails in the 2027-03-03 run that sees the fill: abc is held there.
FILL <- format(seq(as.Date("2026-10-05"), as.Date("2026-10-10"), by = "day"))
held_fill_world <- function(back = NULL, envir = parent.frame()) {
  w <- stop_world(n_fill = 60L, envir = envir)
  for (day in c("2026-10-03", "2026-11-03", "2027-01-03", "2027-02-03")) w$step(day)
  w$env$lp <- rbind(w$env$lp, lp_rows(1, FILL, 2), lp_rows(2, FILL, 2), back)
  w$env$gone <- function(u) url_pub(u) == 2L
  r <- w$step("2027-03-03")
  expect_identical(r$manifest$package_edges, list(abc = "2027-02-01"))
  expect_identical(r$manifest$refetch_from, "2026-09-21")
  w$env$gone <- function(u) FALSE
  w
}
# The start_date of every request for abc's release in a run.
abc_starts <- function(r) unique(url_param(grep("binarypub/2\\?", requested(r$io), value = TRUE),
                                           "start_date"))
# Launchpad revises abc's filled days up.
revise_abc <- function(w) {
  lp <- w$env$lp
  lp$count[lp$pub_id == 2L & lp$day %in% FILL] <- 4L
  w$env$lp <- lp
}

test_that("a package held when a stopped stretch is filled in fetches the filled days in two runs", {
  # abc is held in the run that finds the fill, so the next run is the first
  # to store abc's filled days, and a later one must fetch them again
  # the rest of the stretch is still stopped
  w <- held_fill_world()
  r1 <- w$step("2027-04-03")
  expect_identical(cnt(history_of(w$pub), "abc", "2026-10-07"), 2L)
  expect_identical(r1$manifest$refetch_from, "2026-10-11")
  expect_identical(r1$manifest$package_refetch_from, list(abc = "2026-09-21"))
  revise_abc(w)
  r2 <- w$step("2027-05-03")
  expect_identical(abc_starts(r2), "2026-09-21")
  expect_length(r2$manifest$package_refetch_from, 0L)
  expect_identical(history_of(w$pub), truth_daily(w$env$lp, w$roster, "2027-05-01"))
  # downloads came back with the fill, so no stretch is pending any more
  w2 <- held_fill_world(back = lp_rows(1, format(seq(as.Date("2027-02-10"), as.Date("2027-06-01"),
                                                     by = "day")), 1))
  w2$step("2027-04-03")
  revise_abc(w2)
  expect_identical(abc_starts(w2$step("2027-05-03")), "2026-09-21")
  expect_identical(history_of(w2$pub), truth_daily(w2$env$lp, w2$roster, "2027-05-01"))
  # abc's page answers empty in the run that refreshes it: the next run
  # still fetches the filled days
  w3 <- held_fill_world()
  full <- w3$env$lp
  w3$env$lp <- full[!(full$pub_id == 2L & full$day %in% FILL), ]
  w3$step("2027-04-03")
  w3$env$lp <- full
  expect_identical(abc_starts(w3$step("2027-05-03")), "2026-09-21")
  expect_identical(history_of(w3$pub), truth_daily(full, w3$roster, "2027-05-01"))
})

test_that("a stretch filled in shortly before the edge stays recorded until the quiet after it stops", {
  # counted through 2027-01-01 with a stopped stretch from 2026-09-21 and abc
  # held at 2026-12-15. Launchpad then fills in zoo's 2026-12-20 .. 12-25:
  # the quiet since is 38 days on 2027-02-01, a stall and not a stop
  pub <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-09-20"), by = "day")), 1),
              lp_rows(2, c("2026-08-10", "2026-09-15"), c(3, 2)))
  gap <- list(list(from = "2026-09-21", to = "2027-01-01", kind = "stopped"))
  seed_world(pub, roster, lp, "2027-01-01", edges = list(abc = "2026-12-15"),
             extra = list(detected_gaps = gap, refetch_from = "2026-09-21"))
  live <- rbind(lp, lp_rows(1, format(seq(as.Date("2026-12-20"), as.Date("2026-12-25"), by = "day")), 4))
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    res <- run_q(lp_io(pub, roster, live, now = t_at(day)), out)
    if (isTRUE(res$publish)) publish(out, pub)
    res
  }
  r1 <- step("2027-02-03")               # abc catches up; counted_through cannot go back
  expect_true(r1$publish)
  expect_identical(r1$manifest$counted_through, "2027-01-01")
  expect_length(r1$manifest$package_edges, 0L)
  expect_identical(r1$manifest$detected_gaps, gap)
  expect_identical(r1$manifest$refetch_from, "2026-09-21")
  expect_false(step("2027-03-03")$publish)             # 66 quiet days: still a stall
  r3 <- step("2027-04-03")                              # 97 quiet days: stopped again
  expect_identical(r3$manifest$counted_through, "2027-04-01")
  expect_identical(r3$manifest$detected_gaps,
                   list(list(from = "2026-09-21", to = "2026-12-19"),
                        list(from = "2026-12-26", to = "2027-04-01", kind = "stopped")))
  expect_identical(history_of(pub), truth_daily(live, roster, "2027-04-01"))
})

test_that("a catch-up run leaves a stretch counted as stopped as it is", {
  # zoo was held at 2027-09-01; downloads stopped after 2027-01-01. A run that
  # settles nothing past counted_through only catches zoo up, and must not
  # cut the stretch short at its own last settled day
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-12-01"), as.Date("2027-01-01"), by = "day")), 1),
              lp_rows(2, "2026-12-10", 3))
  gap <- list(list(from = "2027-01-02", to = "2027-10-01", kind = "stopped"))
  seed_world(pub, roster, lp, "2027-10-01", edges = list(zoo = "2027-09-01"),
             extra = list(detected_gaps = gap, refetch_from = "2027-01-02"))
  io <- lp_io(pub, roster, lp, now = t_at("2027-10-02"))              # T = 2027-09-30
  res <- run_q(io, out)
  expect_match(requested(io), "start_date=2027-01-02&end_date=2027-09-30$")
  m <- res$manifest
  expect_identical(m$counted_through, "2027-10-01")
  expect_identical(m$package_edges, list(zoo = "2027-09-30"))
  expect_identical(m$detected_gaps, gap)
  expect_identical(m$refetch_from, "2027-01-02")
})

test_that("a stretch that began inside the days already counted is recorded and fetched from its start", {
  # Launchpad hides every day from 2026-09-30 until 2027-01-15. The October
  # run counts 09-30 and 10-01 as zero (two quiet days are no stall), so when
  # the quiet passes STALL_MAX_DAYS it began before counted_through
  pub <- withr::local_tempdir()
  w <- std_world(through = "2027-03-31")
  visible <- function(day) w$lp[w$lp$day < day & (day >= "2027-01-15" | w$lp$day < "2026-09-30"), ]
  seed_world(pub, w$roster, visible(E_STD), E_STD)
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    io <- lp_io(pub, w$roster, visible(day), now = t_at(day))
    res <- run_q(io, out)
    if (isTRUE(res$publish)) publish(out, pub)
    res$io <- io
    res
  }
  expect_identical(step("2026-10-03")$manifest$counted_through, "2026-10-01")
  expect_false(step("2026-11-03")$publish)
  expect_false(step("2026-12-03")$publish)
  r <- step("2027-01-03")
  expect_true(any(grepl("^::warning::.*no downloads from 2026-09-30 to 2027-01-01", r$stdout)))
  expect_identical(r$manifest$detected_gaps,
                   list(list(from = "2026-09-30", to = "2027-01-01", kind = "stopped")))
  expect_identical(r$manifest$refetch_from, "2026-09-30")
  r2 <- step("2027-02-03")
  expect_match(requested(r2$io), "start_date=2026-09-30&")
  expect_identical(history_of(pub), truth_daily(w$lp, w$roster, "2027-02-01"))
})

test_that("a trailing run of days with no downloads caps counted_through with a warning", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(through = "2026-09-25")   # Launchpad has nothing after 09-25
  seed_world(pub, w$roster, w$lp, E_STD)
  res <- run_q(lp_io(pub, w$roster, w$lp), out)
  expect_true(any(grepl("^::warning::.*2026-09-26 to 2026-10-01", res$stdout)))
  m <- res$manifest
  expect_identical(m$counted_through, "2026-09-25")
  expect_identical(m$summary$latest_date, "2026-09-25")
  expect_length(m$detected_gaps, 0L)
  h <- history_of(out)
  expect_false(any(h$date > "2026-09-25"))
  expect_identical(h, truth_daily(w$lp, w$roster, "2026-09-25"))
})

test_that("a zero run after which downloads resumed is recorded and counted through", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  lp <- w$lp[!(w$lp$day >= "2026-09-10" & w$lp$day <= "2026-09-13"), ]
  seed_world(pub, w$roster, lp, E_STD,
             extra = list(detected_gaps = list(list(from = "2026-02-01", to = "2026-02-05"))))
  res <- run_q(lp_io(pub, w$roster, lp), out)
  m <- res$manifest
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(m$detected_gaps, list(list(from = "2026-02-01", to = "2026-02-05"),
                                         list(from = "2026-09-10", to = "2026-09-13")))
  expect_false(any(grepl("::warning::", res$stdout)))
})

test_that("the known Launchpad hole is neither flagged nor held against counted_through", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  roster <- mk_roster("r-cran-zoo")
  lp <- lp_rows(1, format(c(seq(as.Date("2026-04-01"), as.Date("2026-05-05"), by = "day"),
                            seq(as.Date("2026-07-12"), as.Date("2026-07-22"), by = "day"))), 3)
  seed_world(pub, roster, lp, "2026-05-01")
  res <- run_q(lp_io(pub, roster, lp, now = t_at("2026-07-22")), out)
  expect_identical(res$manifest$counted_through, "2026-07-20")
  expect_length(res$manifest$detected_gaps, 0L)
  expect_false(any(grepl("::warning::", res$stdout)))
  expect_identical(history_of(out), truth_daily(lp, roster, "2026-07-20"))
})

# --- fetch phases ------------------------------------------------------------------

test_that("every request goes through the pool in batches; the serial fetch only retries", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 3L)                              # 6 window releases
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, "2026-03-01", 2))
  seed_world(pub, roster, lp, E_STD)
  io <- lp_io(pub, roster, lp)
  res <- run_q(io, out, update_batch = 4L)
  expect_identical(lengths(io$log$batches), c(4L, 2L, 1L))  # two window batches, one full
  expect_false(any(startsWith(io$log$seq, "fetch ")))
  u <- requested(io)
  expect_match(u[url_pub(u) != 4L], "&start_date=2026-08-02&end_date=2026-10-01$")
  expect_false(grepl("start_date|end_date", u[url_pub(u) == 4L]))
  expect_true(all(vapply(io$log$deadlines, function(d) identical(d, NOW_STD + DEADLINE_MIN * 60),
                         logical(1))))
  expect_true(any(grepl("^window batch 1/2: 4 ok, 0 failed, [0-9.]+ min elapsed$", res$messages)))
  expect_true(any(grepl("^full history batch 1/1: 1 ok, 0 failed", res$messages)))

  out2 <- withr::local_tempdir()
  io2 <- lp_io(pub, roster, lp, pool_fail = function(u) url_pub(u) == 101L)
  run_q(io2, out2, update_batch = 4L)
  serial <- sub("^fetch ", "", grep("^fetch ", io2$log$seq, value = TRUE))
  expect_identical(url_pub(serial), 101L)
  expect_identical(history_of(out2), history_of(out))
})

test_that("the window is fetched and retried before any full-history request", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, "2026-03-01", 2))
  seed_world(pub, roster, lp, E_STD)
  flaky <- function(u) url_pub(u) %in% c(1L, 4L)
  io <- lp_io(pub, roster, lp, pool_fail = flaky)
  run_q(io, out)
  kind <- paste(sub(" .*$", "", io$log$seq), ifelse(url_pub(requested(io)) == 4L, "F", "W"))
  expect_identical(kind, c("many W", "many W", "many W", "fetch W", "many F", "fetch F"))
  expect_identical(history_of(out), truth_daily(lp, roster, "2026-10-01"))
})

test_that("the residual pass replaces a failed item's partial pool pages", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-zoo", version = "3", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(1, "2019-03-01", 100),
              lp_rows(4, c("2019-03-01", "2026-03-01", "2026-09-10"), c(5, 7, 2)))
  seed_world(pub, roster, lp, E_STD)
  # one row a page: the pool gets zoo v1's first window page and v3's first two
  # pages, then fails; the serial retry pages both from the start
  partial <- function(u) (url_pub(u) == 1L && grepl("&ws.start=1$", u)) ||
                         (url_pub(u) == 4L && grepl("&ws.start=2$", u))
  io <- lp_io(pub, roster, lp, page = 1L, pool_fail = partial)
  run_q(io, out)
  h <- history_of(out)
  expect_identical(cnt(h, "zoo", "2019-03-01"), 105L)
  expect_identical(cnt(h, "zoo", "2026-03-01"), 7L)
  expect_identical(h, truth_daily(lp, roster, "2026-10-01"))
  expect_identical(sort(unique(url_pub(sub("^fetch ", "", grep("^fetch ", io$log$seq, value = TRUE))))),
                   c(1L, 4L))
})

test_that("reaching the deadline during the window fetch stops the run with nothing written", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  clock <- NOW_STD
  io <- lp_io(pub, w$roster, w$lp, now = function() clock,
              on_many = function(urls) clock <<- clock + 300 * 60)
  expect_error(run_q(io, out, update_batch = 1L), "deadline")
  expect_length(io$log$batches, 1L)
  expect_nothing_written(out, pub)
})

test_that("no window retry starts near the deadline; the release is held, not the run stopped", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 50L)
  seed_world(pub, w$roster, w$lp, E_STD)
  clock <- NOW_STD
  # the pool uses all but 5 minutes of the deadline and misses zoo v2
  io <- lp_io(pub, w$roster, w$lp, now = function() clock,
              pool_fail = function(u) url_pub(u) == 2L,
              on_many = function(urls) clock <<- NOW_STD + (DEADLINE_MIN - 5) * 60)
  res <- run_q(io, out)
  expect_false(any(startsWith(io$log$seq, "fetch ")))
  expect_true(res$publish)
  expect_identical(res$manifest$package_edges, list(zoo = "2026-09-01"))
  expect_true(any(grepl("window retry: .*1 not retried \\(deadline\\)", res$messages)))
})

test_that("window releases that fail serially until the clock runs low are held, not a deadline stop", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 100L)                       # 103 window releases: 2 may be held
  seed_world(pub, w$roster, w$lp, E_STD)
  clock <- NOW_STD
  bad <- function(u) url_pub(u) %in% c(101L, 102L)
  io <- lp_io(pub, w$roster, w$lp, now = function() clock, pool_fail = bad, serial_fail = bad)
  slow <- io$fetch
  io$fetch <- function(url) { clock <<- clock + 9 * 60; slow(url) }   # one url timing out
  res <- run_q(io, out, deadline_min = 15)
  expect_true(res$publish)
  expect_identical(names(res$manifest$package_edges), c("fill01", "fill02"))
  expect_identical(sum(startsWith(io$log$seq, "fetch ")), 1L)
})

test_that("the window retry stops at its budget and holds what it did not recover", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world(n_fill = 150L)                       # 153 window releases: 3 may be held
  seed_world(pub, w$roster, w$lp, E_STD)
  clock <- NOW_STD
  bad <- function(u) url_pub(u) %in% 101:103
  io <- lp_io(pub, w$roster, w$lp, now = function() clock, pool_fail = bad, serial_fail = bad)
  slow <- io$fetch
  io$fetch <- function(url) { clock <<- clock + 40 * 60; slow(url) }
  res <- run_q(io, out)
  expect_identical(sum(startsWith(io$log$seq, "fetch ")), 2L)   # the third starts past 60 min
  expect_true(res$publish)
  expect_identical(names(res$manifest$package_edges), c("fill01", "fill02", "fill03"))
  expect_true(any(grepl("window retry: .*1 not retried \\(retry budget\\)", res$messages)))
})

test_that("reaching the deadline during the full-history fetch leaves the rest for next run", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster(c("r-cran-zoo", "r-cran-abc"), version = c("3", "2"),
                                      pub_id = 4:5, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, "2026-03-01", 7), lp_rows(5, "2026-03-02", 9))
  seed_world(pub, roster, lp, E_STD)
  clock <- NOW_STD
  io <- lp_io(pub, roster, lp, now = function() clock,
              on_many = function(urls) if (any(url_pub(urls) %in% 4:5)) clock <<- clock + 300 * 60)
  res <- run_q(io, out, update_batch = 1L)
  expect_true(res$publish)
  expect_false(5L %in% url_pub(requested(io)))
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 4L], 1L)
  expect_identical(rel$done[rel$pub_id == 5L], 0L)
  m <- res$manifest
  expect_identical(m$unfetched_releases, 1L)
  expect_false(m$complete)
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(history_of(out), truth_daily(lp, roster, "2026-10-01", 1:4))
  expect_true(any(grepl("deadline.*1 full-history release", res$messages)))
})

# --- held edges when nothing new is settled ---------------------------------------

test_that("with nothing new settled, only packages held at an earlier edge are caught up", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, "2026-10-01", edges = list(zoo = "2026-09-01"))
  io <- lp_io(pub, w$roster, w$lp, now = t_at("2026-10-03"))    # T = E
  res <- run_q(io, out)
  expect_true(res$publish)
  expect_setequal(unique(url_pub(requested(io))), 1:2)
  expect_match(requested(io), "start_date=2026-08-02&end_date=2026-10-01$")
  m <- res$manifest
  expect_identical(m$counted_through, "2026-10-01")
  expect_length(m$package_edges, 0L)
  expect_identical(history_of(out), truth_daily(w$lp, w$roster, "2026-10-01"))
})

test_that("a catch-up run counts a never-fetched release of a package it does not refresh", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, c("2019-03-01", "2026-09-25"), c(5, 7)))
  seed_world(pub, roster, lp, "2026-10-01", edges = list(zoo = "2026-09-01"))
  res <- run_q(lp_io(pub, roster, lp, now = t_at("2026-10-03")), out)   # T = E
  m <- res$manifest
  expect_true(m$complete)
  expect_identical(m$unfetched_releases, 0L)
  h <- history_of(out)
  # abc was not re-fetched (nothing settled past its edge): its stored window
  # days stay, and its new release adds every day through counted_through
  expect_identical(cnt(h, "abc", "2026-09-20"), 8L)
  expect_identical(cnt(h, "abc", "2026-09-25"), 7L)
  expect_identical(h, truth_daily(lp, roster, "2026-10-01"))
  publish(out, pub)
  run_q(lp_io(pub, roster, lp, now = t_at("2026-11-03")), out2)
  expect_identical(history_of(out2), truth_daily(lp, roster, "2026-11-01"))
})

test_that("a catch-up run with T before counted_through moves a held edge only to T", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  # abc is not held and gets a new release with a day on counted_through
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, c("2026-09-25", "2026-10-01"), c(2, 5)))
  seed_world(pub, roster, lp, "2026-10-01", edges = list(zoo = "2026-09-01"))
  res <- run_q(lp_io(pub, roster, lp, now = t_at("2026-10-02")), out)   # T = 2026-09-30
  expect_identical(res$manifest$package_edges, list(zoo = "2026-09-30"))
  expect_identical(res$manifest$counted_through, "2026-10-01")
  expect_false(res$manifest$complete)
  expect_identical(history_of(out, "zoo"), truth_daily(lp, roster, "2026-09-30", 1:2))
  # abc keeps its edge, so its new release counts through it
  expect_identical(history_of(out, "abc"), truth_daily(lp, roster, "2026-10-01", 3:4))
})

test_that("a stall right after counted_through with a held edge still counts new releases", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, c("2019-03-01", "2026-08-25"), c(5, 7)))
  lp <- lp[lp$day <= E_STD, ]                          # Launchpad reports nothing after E
  seed_world(pub, roster, lp, E_STD, edges = list(zoo = "2026-08-15"))
  res <- run_q(lp_io(pub, roster, lp), out)
  expect_true(res$publish)
  expect_true(any(grepl("^::warning::.*2026-09-02 to 2026-10-01", res$stdout)))
  m <- res$manifest
  expect_identical(m$counted_through, E_STD)
  expect_length(m$package_edges, 0L)
  expect_true(m$complete)
  expect_identical(history_of(out), truth_daily(lp, roster, E_STD))
})

test_that("a stall right after counted_through with nothing held publishes nothing", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- rbind(w$roster, mk_roster("r-cran-abc", version = "2", pub_id = 4L, done = 0L))
  lp <- rbind(w$lp, lp_rows(4, "2019-03-01", 5))
  lp <- lp[lp$day <= E_STD, ]
  seed_world(pub, roster, lp, E_STD)
  io <- lp_io(pub, roster, lp)
  res <- run_q(io, out)
  expect_false(res$publish)
  expect_true(any(grepl("^::warning::.*2026-09-02 to 2026-10-01", res$stdout)))
  expect_false(4L %in% url_pub(requested(io)))          # the full-history fetch never starts
  expect_nothing_written(out, pub)
})

# --- dormant probe -----------------------------------------------------------------

dormant_world <- function(n = 52L) {
  roster <- mk_roster(sprintf("r-cran-d%02d", seq_len(n)))
  lp <- lp_rows(seq_len(n), format(as.Date("2024-01-01") + seq_len(n) - 1L), 1)
  list(roster = roster, lp = lp)
}

test_that("with every release dormant, a quiet probe writes a dormant heartbeat", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- dormant_world()
  prev <- seed_world(pub, w$roster, w$lp, E_STD)
  prev <- read_manifest(pub)
  io <- lp_io(pub, w$roster, w$lp)
  res <- run_q(io, out)
  expect_true(res$publish)
  u <- requested(io)
  expect_setequal(url_pub(u), 3:52)                      # the 50 last downloaded
  expect_match(u, "&start_date=2026-09-01$")
  m <- read_manifest(out)
  expect_identical(m$source_kind, "dormant")
  expect_identical(m$last_checked, iso(NOW_STD))
  expect_identical(m$counted_through, E_STD)
  expect_identical(m$changed_shards, prev$changed_shards)
  expect_identical(res$changed_shards, unlist(prev$changed_shards))
  # the assets are already on the release: only the manifest is uploaded
  expect_identical(readLines(file.path(out, UPLOAD_LIST)), character(0))
  expect_identical(m$shards, prev$shards)
  expect_silent(verify_release(out, m))
})

test_that("a dormant release with new downloads stops the run and asks for a backfill", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- dormant_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  live <- rbind(w$lp, lp_rows(52, "2026-09-20", 4))
  expect_error(run_q(lp_io(pub, w$roster, live), out),
               "dormant releases have new downloads.*run backfill.yml")
  expect_nothing_written(out, pub)
})

test_that("with nothing active, dormant releases are probed even when others await a full fetch", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- dormant_world()
  roster <- rbind(w$roster, mk_roster("r-cran-new", pub_id = 99L, done = 2L))
  seed_world(pub, roster, w$lp, E_STD)
  # d52 was downloaded again: counting on without asking would lose it
  live <- rbind(w$lp, lp_rows(52, "2026-09-20", 4))
  expect_error(run_q(lp_io(pub, roster, live), out),
               "dormant releases have new downloads.*run backfill.yml")
  expect_nothing_written(out, pub)
  # a quiet probe lets the run go on to the release that awaits its second answer
  io <- lp_io(pub, roster, w$lp)
  res <- run_q(io, out2)
  expect_true(res$publish)
  u <- requested(io)
  expect_setequal(url_pub(u), c(3:52, 99L))
  expect_match(u[url_pub(u) != 99L], "&start_date=2026-09-01$")
  rel <- load_releases(file.path(out2, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[rel$pub_id == 99L], 1L)
})

test_that("the dormant probe starts at a stretch still counted as stopped", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  w <- dormant_world()                           # last downloads 2024-01-01 .. 2024-02-21
  gap <- list(list(from = "2024-02-22", to = E_STD, kind = "stopped"))
  seed_world(pub, w$roster, w$lp, E_STD, extra = list(detected_gaps = gap))
  io <- lp_io(pub, w$roster, w$lp)
  res <- run_q(io, out)
  expect_identical(res$manifest$source_kind, "dormant")
  expect_match(requested(io), "&start_date=2024-02-22$")
  # Launchpad fills in a day of the stretch for a dormant release
  live <- rbind(w$lp, lp_rows(52, "2025-03-01", 4))
  expect_error(run_q(lp_io(pub, w$roster, live), out2),
               "dormant releases have new downloads.*run backfill.yml")
})

test_that("with nothing active, a quiet probe carries a stopped stretch on to the new edge", {
  # a release awaits its second answer, so the run counts on past the probe;
  # the stretch must go on as stopped or the next probe would start after it
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- dormant_world()
  roster <- rbind(w$roster, mk_roster("r-cran-new", pub_id = 99L, done = 2L))
  gap <- list(list(from = "2024-02-22", to = E_STD, kind = "stopped"))
  seed_world(pub, roster, w$lp, E_STD, extra = list(detected_gaps = gap))
  res <- run_q(lp_io(pub, roster, w$lp), out)
  m <- res$manifest
  expect_identical(m$counted_through, "2026-10-01")
  expect_identical(m$detected_gaps, list(list(from = "2024-02-22", to = "2026-10-01", kind = "stopped")))
  expect_identical(m$refetch_from, "2024-02-22")
})

# --- reclassify-only ---------------------------------------------------------------

test_that("reclassify_only rebuilds identity + summary from history with zero Launchpad calls, touching no year shard", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  lp <- rbind(w$lp, lp_rows(3, "2019-03-01", 4))
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(zoo = "zoo", abc = "abc"),
                       bioc = c(biobase = "Biobase"), states = c(zoo = "archived"))
  seed_world(pub, w$roster, lp, E_STD, edges = list(abc = "2026-08-20"),
             extra = list(detected_gaps = list(list(from = "2026-02-01", to = "2026-02-05")),
                          refetch_from = "2026-07-20", package_refetch_from = list(zoo = "2026-07-01")))
  # the last publish listed only the 2019 year shard as changed
  prev <- read_manifest(pub)
  prev$changed_shards <- list("c2d4u-downloads-2019.db", "c2d4u-downloads-recent.db",
                              "c2d4u-downloads-summary.db")
  write_manifest(file.path(pub, "manifest.json"), prev)
  year_bytes <- function(dir) tools::md5sum(file.path(dir, c("c2d4u-downloads-2019.db", "c2d4u-downloads-2026.db")))
  before <- year_bytes(pub)
  io <- fake_io(pub, ledger = led, now = NOW_STD)
  io$fetch <- io$fetch_many <- function(...) stop("must not call Launchpad in reclassify mode")
  res <- run_q(io, out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L)

  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$identity_state[s$package == "zoo"], "archived")
  emb <- load_summary(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(emb$identity_state[emb$package == "zoo"], "archived")

  # the year shards named by the previous publish stay listed, so a loader that
  # has not read them yet still does; none is rewritten
  expect_setequal(res$changed_shards, c("c2d4u-downloads-2019.db", "c2d4u-downloads-recent.db",
                                        "c2d4u-downloads-summary.db"))
  expect_identical(readLines(file.path(out, UPLOAD_LIST)),
                   c("c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db"))
  expect_identical(unname(year_bytes(out)), unname(before))
  m <- read_manifest(out)
  expect_identical(m$source_kind, "reclassify")
  expect_identical(m$last_checked, prev$last_checked)
  expect_identical(m$last_changed, iso(NOW_STD))
  expect_identical(m$counted_through, prev$counted_through)
  expect_identical(m$summary$latest_date, prev$summary$latest_date)
  expect_identical(m$package_edges, prev$package_edges)
  expect_identical(m$detected_gaps, prev$detected_gaps)
  # the refetch floors are carried, so the days they cover still get their second fetch
  expect_identical(m$refetch_from, "2026-07-20")
  expect_identical(m$package_refetch_from, list(zoo = "2026-07-01"))
  expect_identical(m$history_method, "summed")
  expect_identical(m$summary$active_releases, 0L)
  expect_silent(verify_release(out, m))
  # the summary stays anchored on counted_through: 08-02..09-01 is 31 days of
  # zoo v1 plus zoo v2's 1 on 08-15
  expect_identical(s$total_30d[s$package == "zoo"], 2L * 31L + 1L)
})

test_that("reclassify_only anchors a held package's summary windows on its own edge", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(zoo = "zoo", abc = "abc"))
  seed_world(pub, w$roster, w$lp, E_STD, edges = list(zoo = "2026-08-15"))
  io <- fake_io(pub, ledger = led, now = NOW_STD)
  run_q(io, out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L)
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  # zoo's data stops at 2026-08-15: 07-16..08-15 is 31 days of v1 plus v2's 1
  expect_identical(s$total_30d[s$package == "zoo"], 2L * 31L + 1L)
  expect_identical(s$total_30d[s$package == "abc"], 3L)
})

test_that("reclassify_only republishes a legacy release without passing it off as summed", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(zoo = "zoo", abc = "abc"))
  seed_world(pub, w$roster, w$lp, E_STD, extra = list(history_method = NULL, counted_through = NULL))
  io <- fake_io(pub, ledger = led, now = NOW_STD)
  io$fetch <- io$fetch_many <- function(...) stop("must not call Launchpad in reclassify mode")
  res <- run_q(io, out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L)
  m <- read_manifest(out)
  expect_null(m$history_method)
  expect_null(m$counted_through)
  expect_identical(m$summary$latest_date, E_STD)
  expect_false(m$complete)
  # and the monthly update still refuses it
  expect_error(run_q(lp_io(out, w$roster, w$lp), withr::local_tempdir()), "not a summed history")
})

test_that("reclassify_only errors when there is no existing roster to reclassify", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  expect_error(
    run_update(fake_io(pub), out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L),
    "reclassify-only needs an existing roster")
})

test_that("reclassify_only errors when the identity ledger is unreachable", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  io <- fake_io(pub, fail_identity = TRUE, now = NOW_STD)
  expect_error(
    run_update(io, out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L),
    "ledger")
})

# --- identity enrichment on an update ------------------------------------------------

test_that("an update enriches canonical_name and identity_state from the ledger", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  seed_world(pub, w$roster, w$lp, E_STD)
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(zoo = "Zoo", abc = "abc"),
                       bioc = c(biobase = "Biobase"), states = c(zoo = "archived", abc = "live"))
  run_q(lp_io(pub, w$roster, w$lp, ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$canonical_name[s$package == "zoo"], "Zoo")
  expect_identical(s$identity_state[s$package == "zoo"], "archived")
  rel <- load_releases(file.path(out, "c2d4u-downloads-recent.db"))
  expect_identical(unique(rel$identity_state[rel$package == "zoo"]), "archived")
})

test_that("an update degrades honestly when the ledger is unreachable, keeping persisted identity", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- w$roster
  roster$identity_state <- c("archived", "archived", NA)
  seed_world(pub, roster, w$lp, E_STD)
  res <- run_q(lp_io(pub, roster, w$lp, fail_identity = TRUE), out, live_floor = 1L, bioc_floor = 1L)
  expect_true(res$publish)
  expect_true(any(grepl("identity ledger unavailable", res$messages)))
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_setequal(s$package, c("zoo", "abc"))           # no row dropped
  expect_identical(s$canonical_name[s$package == "zoo"], "zoo")
  expect_identical(s$identity_state[s$package == "zoo"], "archived")   # never regressed
  expect_true(is.na(s$identity_state[s$package == "abc"]))              # honest unknown
})

test_that("an update degrades when the identity size gate fails", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  w <- std_world()
  roster <- w$roster; roster$identity_state <- NA_character_
  seed_world(pub, roster, w$lp, E_STD)
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(zoo = "Zoo"), bioc = c(biobase = "Biobase"))
  run_q(lp_io(pub, roster, w$lp, ledger = led), out, live_floor = 999999L, bioc_floor = 1L)
  s <- load_summary(file.path(out, "c2d4u-downloads-summary.db"))
  expect_identical(s$canonical_name[s$package == "zoo"], "zoo")   # token fallback (gate failed)
  expect_true(all(is.na(s$identity_state)))
})

# --- the backfill and the update build on each other --------------------------

# Backfill `roster` as backfill.yml does, from what Launchpad (`lp`) holds on
# the day `fetched`: fetch N shards, then merge them into d/out, checked against
# the release published in `pub` (none by default).
backfill_world <- function(d, roster, lp, fetched, N = 2L, pub = file.path(d, "none")) {
  roster$done <- 0L; roster$last_day <- NA_character_
  rp <- write_roster(file.path(d, ROSTER_FILE), roster)
  seen <- lp[lp$day < fetched, , drop = FALSE]
  io <- fake_io(pub, now = t_at(fetched, "01:00:00"))
  io$fetch <- function(url) lp_answer(url, seen, roster)
  io$fetch_many <- function(urls, deadline = Inf) lapply(urls, io$fetch)
  parts <- file.path(d, "parts"); dir.create(parts)
  for (i in seq_len(N) - 1L)
    file.copy(run_fetch_shard(io, file.path(d, "fetch"), rp, i, N), parts)
  run_merge(io, file.path(d, "out"), parts, N = N, roster_path = rp)
  file.path(d, "out")
}

test_that("the update builds on a backfill's release, and a later backfill on the update's", {
  w <- std_world(through = "2026-11-02")
  quietly(bf1 <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-09-03"))
  expect_identical(read_manifest(bf1)$counted_through, E_STD)
  expect_identical(history_of(bf1), truth_daily(w$lp, w$roster, E_STD))

  up1 <- withr::local_tempdir()
  res1 <- run_q(lp_io(bf1, w$roster, w$lp), up1)
  expect_true(res1$publish)
  expect_identical(res1$manifest$counted_through, "2026-10-01")
  expect_true(res1$manifest$complete)
  expect_identical(history_of(up1), truth_daily(w$lp, w$roster, "2026-10-01"))

  up2 <- withr::local_tempdir()
  res2 <- run_q(lp_io(up1, w$roster, w$lp, now = t_at("2026-11-03")), up2)
  expect_identical(res2$manifest$counted_through, "2026-11-01")
  expect_identical(history_of(up2), truth_daily(w$lp, w$roster, "2026-11-01"))
  expect_silent(verify_release(up2, read_manifest(up2)))

  # A backfill after the updates is checked against the update's release,
  # through the update's counted_through, and publishes the same sums.
  msgs <- testthat::capture_messages(
    bf2 <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-11-04", pub = up2))
  expect_true(any(grepl(paste("merge: 0 of 2 packages (0.0%) count fewer downloads through",
                              "2026-11-01 than the published release"), msgs, fixed = TRUE)))
  expect_identical(history_of(bf2), truth_daily(w$lp, w$roster, "2026-11-02"))
})

test_that("a backfill merge re-run after a monthly update published stops instead of going back", {
  # zoo is downloaded every day; 150 quiet packages are not. The update counts
  # zoo's days past the backfill's settled day, and 1 package of 151 counting
  # fewer is within the dominance tolerance
  roster <- rbind(mk_roster("r-cran-zoo"),
                  mk_roster(sprintf("r-cran-q%03d", 1:150), pub_id = 100L + 1:150))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-11-02"), by = "day")), 2),
              lp_rows(100L + 1:150, "2026-08-10", 3))
  d <- withr::local_tempdir()
  quietly(bf <- backfill_world(d, roster, lp, fetched = "2026-09-03"))
  up <- withr::local_tempdir()
  res <- run_q(lp_io(bf, roster, lp), up)
  expect_identical(res$manifest$counted_through, "2026-10-01")
  # only the merge job is re-run, over the same parts, against the update's release
  again <- file.path(d, "again")
  err <- expect_error(quietly(run_merge(fake_io(up, now = t_at("2026-10-04")), again,
                                        file.path(d, "parts"), N = 2L,
                                        roster_path = file.path(d, ROSTER_FILE))),
                      "predate the published release")
  expect_match(conditionMessage(err), "2026-09-01.*2026-10-01")
  expect_length(list.files(again), 0L)
})

test_that("a release a backfill saw empty is fetched in full by the next update", {
  w <- std_world(through = "2026-11-02")
  roster <- rbind(w$roster, mk_roster(c("r-cran-abc", "r-cran-nil"), version = c("2", "1"),
                                      pub_id = 4:5))
  lp <- rbind(w$lp, lp_rows(4, c("2020-03-01", "2026-08-25"), c(6, 2)))
  # the backfill got a transient empty page for abc v2; nil has no downloads
  quietly(bf <- backfill_world(withr::local_tempdir(), roster, lp[lp$pub_id != 4L, ],
                               fetched = "2026-09-03"))
  rel <- load_releases(file.path(bf, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[match(4:5, rel$pub_id)], c(2L, 2L))
  m <- read_manifest(bf)
  expect_identical(m$unfetched_releases, 0L)
  expect_identical(m$empty_once_releases, 2L)
  expect_false(m$complete)
  up <- withr::local_tempdir()
  res <- run_q(lp_io(bf, roster, lp), up)
  expect_identical(history_of(up), truth_daily(lp, roster, "2026-10-01"))
  rel <- load_releases(file.path(up, "c2d4u-downloads-recent.db"))
  expect_identical(rel$done[match(4:5, rel$pub_id)], c(1L, 1L))
  expect_identical(rel$last_day[match(4:5, rel$pub_id)], c("2026-08-25", NA))
  expect_identical(res$manifest$empty_once_releases, 0L)
  expect_true(res$manifest$complete)
})

test_that("an update soon after a backfill keeps the backfill's year shards listed as changed", {
  w <- std_world(through = "2026-11-02")
  w$lp <- rbind(w$lp, lp_rows(1, c("2024-05-01", "2025-05-01"), c(40, 50)))
  quietly(bf <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-10-02"))
  yrs <- sprintf("c2d4u-downloads-%s.db", 2024:2026)
  rest <- c("c2d4u-downloads-recent.db", "c2d4u-downloads-summary.db")
  expect_identical(unlist(read_manifest(bf)$changed_shards), c(yrs, rest))
  # the loader reads the current manifest's year shards once a day, and this
  # update publishes a day after the backfill: the 2024 and 2025 shards it did
  # not rebuild must stay listed, but are not uploaded again
  up <- withr::local_tempdir()
  res <- run_q(lp_io(bf, w$roster, w$lp, now = t_at("2026-10-03", "09:00:00")), up)
  expect_identical(res$manifest$counted_through, "2026-10-01")
  expect_identical(unlist(res$manifest$changed_shards), c(yrs, rest))
  expect_identical(res$changed_shards, c(yrs, rest))
  expect_identical(readLines(file.path(up, UPLOAD_LIST)), c(yrs[3], rest))
  expect_silent(verify_release(up, read_manifest(up)))
  # a month on they have been loaded, so the next update lists only its own
  publish(up, bf)
  up2 <- withr::local_tempdir()
  res2 <- run_q(lp_io(bf, w$roster, w$lp, now = t_at("2026-11-03")), up2)
  expect_identical(unlist(res2$manifest$changed_shards), c(yrs[3], rest))
})

test_that("a backfill fetched during a Launchpad stall counts through the day before it", {
  # Launchpad reports nothing after 2026-09-01 until it fills the days in on
  # 10-25; the backfill is fetched on 10-20, in the middle of the stall
  w <- std_world(through = "2027-01-31")
  visible <- function(day) w$lp[w$lp$day < day & (day >= "2026-10-25" | w$lp$day <= E_STD), ]
  pub <- withr::local_tempdir()
  seed_world(pub, w$roster, w$lp[w$lp$day <= E_STD, ], E_STD)
  step <- function(day) {
    out <- withr::local_tempdir(.local_envir = parent.frame(2))
    res <- run_q(lp_io(pub, w$roster, visible(day), now = t_at(day)), out)
    if (isTRUE(res$publish)) publish(out, pub)
    res
  }
  expect_false(step("2026-10-03")$publish)
  txt <- utils::capture.output(msgs <- testthat::capture_messages(
    bf <- backfill_world(withr::local_tempdir(), w$roster, visible("2026-10-20"),
                         fetched = "2026-10-20", pub = pub)))
  expect_true(any(grepl("^::warning::.*no downloads from 2026-09-02 to 2026-10-18", txt)))
  m <- read_manifest(bf)
  expect_identical(m$counted_through, E_STD)
  expect_length(m$detected_gaps, 0L)
  publish(bf, pub)
  for (day in c("2026-11-03", "2026-12-03", "2027-01-03")) r <- step(day)
  expect_identical(r$manifest$counted_through, "2027-01-01")
  expect_identical(history_of(pub), truth_daily(w$lp, w$roster, "2027-01-01"))
})

test_that("a backfill fetched after downloads stopped records the stretch, and later fills count", {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-abc"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2027-01-01"), as.Date("2027-03-20"), by = "day")), 1),
              lp_rows(2, "2027-03-10", 3))
  live <- rbind(lp, lp_rows(1, format(seq(as.Date("2027-04-01"), as.Date("2027-04-05"), by = "day")), 4))
  run_after <- function(bf, day) {
    out <- withr::local_tempdir(.local_envir = parent.frame())
    io <- lp_io(bf, roster, live, now = t_at(day))
    res <- run_q(io, out)
    res$io <- io; res$out <- out
    res
  }
  # past STALL_MAX_DAYS when it is fetched: the stretch is recorded from its
  # first quiet day, and the next update fetches it again from there
  txt <- utils::capture.output(suppressMessages(
    bf <- backfill_world(withr::local_tempdir(), roster, lp, fetched = "2027-07-03")))
  expect_true(any(grepl("^::warning::.*no downloads from 2027-03-21 to 2027-07-01", txt)))
  m <- read_manifest(bf)
  expect_identical(m$counted_through, "2027-07-01")
  expect_identical(m$detected_gaps, list(list(from = "2027-03-21", to = "2027-07-01", kind = "stopped")))
  expect_identical(m$refetch_from, "2027-03-21")
  r <- run_after(bf, "2027-08-03")
  expect_match(requested(r$io), "start_date=2027-03-21&")
  expect_identical(history_of(r$out), truth_daily(live, roster, "2027-08-01"))
  # fetched before the quiet passed it: counted through the last download,
  # and the update records the stretch once it does
  quietly(bf2 <- backfill_world(withr::local_tempdir(), roster, lp, fetched = "2027-06-03"))
  expect_identical(read_manifest(bf2)$counted_through, "2027-03-20")
  up <- withr::local_tempdir()
  r1 <- run_q(lp_io(bf2, roster, lp, now = t_at("2027-07-03")), up)
  expect_identical(r1$manifest$detected_gaps,
                   list(list(from = "2027-03-21", to = "2027-07-01", kind = "stopped")))
  r2 <- run_after(up, "2027-08-03")
  expect_identical(history_of(r2$out), truth_daily(live, roster, "2027-08-01"))
})

test_that("a backfill of an archive whose downloads stopped long ago leaves the probe the whole stretch", {
  w <- dormant_world()                           # last downloads 2024-01-01 .. 2024-02-21
  roster <- rbind(w$roster, mk_roster("r-cran-new", pub_id = 99L))
  quietly(bf <- backfill_world(withr::local_tempdir(), roster, w$lp, fetched = "2026-09-03"))
  expect_identical(read_manifest(bf)$detected_gaps,
                   list(list(from = "2024-02-22", to = E_STD, kind = "stopped")))
  up <- withr::local_tempdir()
  r1 <- run_q(lp_io(bf, roster, w$lp), up)        # asks new again; the dormant probe is quiet
  expect_identical(r1$manifest$counted_through, "2026-10-01")
  expect_identical(r1$manifest$refetch_from, "2024-02-22")
  # Launchpad fills in a day of the stretch for a dormant release
  live <- rbind(w$lp, lp_rows(52, "2025-03-01", 4))
  expect_error(run_q(lp_io(up, roster, live, now = t_at("2026-11-03")), withr::local_tempdir()),
               "dormant releases have new downloads.*run backfill.yml")
})

test_that("a backfill after the run that held a package keeps that package's floor", {
  # abc is held in the run that finds days filled in inside the stopped
  # stretch, and a backfill is the next run: it is the first to store abc's
  # filled days, so the update after it fetches them again
  w <- held_fill_world()
  quietly(bf <- backfill_world(withr::local_tempdir(), w$roster, w$env$lp, fetched = "2027-03-10",
                               pub = w$pub))
  m <- read_manifest(bf)
  expect_identical(m$refetch_from, "2026-10-11")
  expect_identical(m$package_refetch_from, list(abc = "2026-09-21"))
  expect_length(m$package_edges, 0L)
  publish(bf, w$pub)
  revise_abc(w)
  r <- w$step("2027-04-03")
  expect_identical(abc_starts(r), "2026-09-21")
  expect_identical(history_of(w$pub), truth_daily(w$env$lp, w$roster, "2027-04-01"))
})

test_that("a release first downloaded after the backfill's settled day is counted the next month", {
  roster <- mk_roster(c("r-cran-zoo", "r-cran-new"))
  lp <- rbind(lp_rows(1, format(seq(as.Date("2026-06-01"), as.Date("2026-08-10"), by = "day")), 2),
              lp_rows(2, c("2026-07-05", "2026-07-20"), c(4, 6)))
  # fetched on 2026-07-06, so counted through 2026-07-04: new's 07-05 is
  # returned but not counted, and still makes it a downloaded release
  quietly(bf <- backfill_world(withr::local_tempdir(), roster, lp, fetched = "2026-07-06"))
  rel <- load_releases(file.path(bf, "c2d4u-downloads-recent.db"))
  expect_identical(rel$last_day[rel$pub_id == 2L], "2026-07-05")
  expect_identical(nrow(history_of(bf, "new")), 0L)
  up <- withr::local_tempdir()
  res <- run_q(lp_io(bf, roster, lp, now = t_at("2026-08-03")), up)
  expect_identical(res$manifest$summary$excluded_never_downloaded, 0L)
  expect_identical(history_of(up), truth_daily(lp, roster, "2026-08-01"))
})

# Upload a run's database assets to `pub` but not its manifest.json, as a
# publish stopped before its last upload leaves the release.
publish_torn <- function(out, pub, files) {
  for (f in files) file.copy(file.path(out, f), file.path(pub, f), overwrite = TRUE)
}

test_that("a backfill replaces an update publish torn before its manifest landed", {
  w <- std_world(through = "2026-11-02")
  pub <- withr::local_tempdir(); up <- withr::local_tempdir()
  seed_world(pub, w$roster, w$lp[w$lp$day <= E_STD, ], E_STD)
  res <- run_q(lp_io(pub, w$roster, w$lp), up)                  # counted through 2026-10-01
  publish_torn(up, pub, res$changed_shards)
  expect_error(run_q(lp_io(pub, w$roster, w$lp, now = t_at("2026-10-05")), withr::local_tempdir()),
               "torn or foreign release")
  msgs <- testthat::capture_messages(
    bf <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-10-05", pub = pub))
  expect_true(any(grepl("merge: 0 of 2 packages (0.0%) count fewer downloads through 2026-10-01",
                        msgs, fixed = TRUE)))
  expect_identical(history_of(bf), truth_daily(w$lp, w$roster, "2026-10-03"))
})

test_that("a backfill replaces a recovery backfill torn over the legacy release", {
  w <- std_world(through = "2026-11-02")
  pub <- withr::local_tempdir()
  seed_world(pub, w$roster, w$lp[w$lp$day <= "2026-05-05", ], "2026-05-05",
             extra = list(history_method = NULL, counted_through = NULL))
  quietly(bf1 <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-09-25", pub = pub))
  publish_torn(bf1, pub, unlist(read_manifest(bf1)$changed_shards))
  quietly(bf2 <- backfill_world(withr::local_tempdir(), w$roster, w$lp, fetched = "2026-09-27", pub = pub))
  expect_identical(history_of(bf2), truth_daily(w$lp, w$roster, "2026-09-25"))
})
