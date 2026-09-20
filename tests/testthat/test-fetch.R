# Offline tests for the fetch layer: the deadline-bounded concurrent pool, the
# deadline-aware paginator, and the retry policy of the serial fetch. curl is
# mocked, so nothing here touches the network.

# A fake curl multi layer on a fake clock. multi_add records each handle's done
# callback in the order fetch_pool adds them; multi_run answers every pending
# handle of its pool with HTTP 200 and body "b<k>" (k = add order), then moves
# the clock forward by `step` seconds. Timeouts passed to multi_run are logged.
fake_multi <- function(t0, step) {
  st <- new.env()
  st$t <- t0; st$added <- 0L; st$pending <- list(); st$timeouts <- numeric(0)
  st$now <- function() st$t
  st$multi_add <- function(handle, done = NULL, fail = NULL, data = NULL, pool = NULL) {
    st$added <- st$added + 1L
    st$pending[[length(st$pending) + 1L]] <- list(k = st$added, done = done)
    invisible(handle)
  }
  st$multi_run <- function(timeout = Inf, poll = FALSE, pool = NULL) {
    st$timeouts <- c(st$timeouts, timeout)
    for (p in st$pending)
      p$done(list(status_code = 200L, content = charToRaw(paste0("b", p$k))))
    st$pending <- list()
    st$t <- st$t + step
    invisible(list(success = 0L, error = 0L, pending = 0L))
  }
  st
}

test_that("fetch_pool makes no request once the deadline has passed", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  local_mocked_bindings(
    multi_run = function(...) stop("multi_run must not be called past the deadline"),
    .package = "curl")
  out <- fetch_pool(c("u1", "u2"), pool = 2L, passes = 3L,
                    deadline = t0 - 1, now = function() t0)
  expect_length(out, 2L)
  expect_true(all(vapply(out, is.null, logical(1))))
})

test_that("fetch_pool bounds each block by the time left and skips blocks past the deadline", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  fm <- fake_multi(t0, step = 60)
  local_mocked_bindings(multi_add = fm$multi_add, multi_run = fm$multi_run, .package = "curl")
  urls <- paste0("https://example.invalid/", 1:5)
  out <- fetch_pool(urls, pool = 2L, passes = 3L, block = 2L,
                    deadline = t0 + 90, now = fm$now)
  # block 1 starts with 90 s left, block 2 with 30 s left, block 3 never starts
  expect_identical(fm$timeouts, c(90, 30))
  expect_identical(out[1:4], list("b1", "b2", "b3", "b4"))
  expect_null(out[[5]])
})

test_that("fetch_pool without a deadline runs every block unbounded", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  fm <- fake_multi(t0, step = 60)
  local_mocked_bindings(multi_add = fm$multi_add, multi_run = fm$multi_run, .package = "curl")
  out <- fetch_pool(paste0("u", 1:3), pool = 2L, passes = 1L, block = 2L, now = fm$now)
  expect_identical(fm$timeouts, c(Inf, Inf))
  expect_identical(out, list("b1", "b2", "b3"))
})

test_that("fetch_pool skips its retry passes when the deadline leaves no time for them", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  fm <- fake_multi(t0, step = 60)
  # every response is a 503, so each url stays NULL after the first pass
  fm$multi_run <- function(timeout = Inf, poll = FALSE, pool = NULL) {
    fm$timeouts <- c(fm$timeouts, timeout)
    for (p in fm$pending) p$done(list(status_code = 503L, content = raw(0)))
    fm$pending <- list(); fm$t <- fm$t + 60
    invisible(NULL)
  }
  local_mocked_bindings(multi_add = fm$multi_add, multi_run = fm$multi_run, .package = "curl")
  out <- fetch_pool(c("u1", "u2"), pool = 2L, passes = 4L,
                    deadline = t0 + 61, now = fm$now)
  expect_length(fm$timeouts, 1L)   # no backoff sleep and no second pass
  expect_true(all(vapply(out, is.null, logical(1))))
})

test_that("fetch_pool cancels the requests still in flight when the deadline ends a block", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  fm <- fake_multi(t0, step = 60)
  fm$multi_add <- function(handle, done = NULL, fail = NULL, data = NULL, pool = NULL) {
    fm$added <- fm$added + 1L
    fm$pending[[length(fm$pending) + 1L]] <- list(k = fm$added, done = done, h = handle)
    invisible(handle)
  }
  # only the first request answers before multi_run returns at its timeout
  fm$multi_run <- function(timeout = Inf, poll = FALSE, pool = NULL) {
    fm$pending[[1]]$done(list(status_code = 200L, content = charToRaw("b1")))
    fm$pending <- fm$pending[-1]
    fm$t <- fm$t + 60
    invisible(NULL)
  }
  cancelled <- 0L
  local_mocked_bindings(
    multi_add = fm$multi_add, multi_run = fm$multi_run,
    multi_list = function(pool = NULL) lapply(fm$pending, `[[`, "h"),
    multi_cancel = function(handle) {
      cancelled <<- cancelled + 1L
      fm$pending <- Filter(function(p) !identical(p$h, handle), fm$pending)
      invisible(handle)
    },
    .package = "curl")
  out <- fetch_pool(c("u1", "u2", "u3"), pool = 3L, passes = 1L, deadline = t0 + 30, now = fm$now)
  expect_identical(cancelled, 2L)
  expect_length(fm$pending, 0L)
  expect_identical(out[[1]], "b1")
  expect_null(out[[2]]); expect_null(out[[3]])
})

# A one-entry counts page for r-cran-a version 1 (counts_json is in helper-setup.R),
# in a collection of total_size rows.
counts_body <- function(day, count, next_link = NULL, total_size = length(day))
  counts_json("r-cran-a", "1", day, count, next_link = next_link, total_size = total_size)

test_that("fetch_paginated starts no wave once the deadline has passed", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  called <- 0L
  fm <- function(urls) { called <<- called + 1L; lapply(urls, function(u) counts_body("2026-09-01", 1L)) }
  res <- fetch_paginated(fm, c("u1", "u2"), parse_counts_page, "rows",
                         deadline = t0, now = function() t0)
  expect_identical(called, 0L)
  expect_identical(res$ok, c(FALSE, FALSE))
})

test_that("fetch_paginated stops following pages at the deadline, leaving those items not ok", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  clock <- t0
  pages <- list(u1 = counts_body("2026-09-01", 4L),
                u2 = counts_body("2026-09-02", 5L, next_link = "u2p2", total_size = 2L),
                u2p2 = counts_body("2026-09-01", 6L, total_size = 2L))
  # a plain function(urls) fake: the deadline is enforced between waves
  fm <- function(urls) { clock <<- clock + 120; lapply(urls, function(u) pages[[u]]) }
  res <- fetch_paginated(fm, c("u1", "u2"), parse_counts_page, "rows",
                         deadline = t0 + 60, now = function() clock)
  expect_identical(res$ok, c(TRUE, FALSE))
  expect_identical(res$data[[1]]$count, 4L)
  expect_identical(res$data[[2]]$count, 5L)   # partial: page 2 never fetched
})

test_that("fetch_paginated forwards the deadline to a fetch_many that accepts one", {
  t0 <- as.POSIXct("2026-10-03 06:00:00", tz = "UTC")
  seen <- list()
  fm <- function(urls, deadline = Inf) {
    seen[[length(seen) + 1L]] <<- deadline
    lapply(urls, function(u) counts_body("2026-09-01", 1L))
  }
  res <- fetch_paginated(fm, "u1", parse_counts_page, "rows",
                         deadline = t0 + 600, now = function() t0)
  expect_true(res$ok)
  expect_identical(seen, list(t0 + 600))
  # the default (no deadline) keeps the plain call shape working
  plain <- fetch_paginated(function(urls) lapply(urls, function(u) counts_body("2026-09-01", 2L)),
                           "u1", parse_counts_page, "rows")
  expect_true(plain$ok)
})

test_that("an item whose pages do not add up to total_size is not ok", {
  # Launchpad reports the size of the whole collection on every page. A later
  # page that answers empty with no next link ends the paging early; taking
  # what came back would settle a release with only its newest pages. Pages
  # are cut by offset, newest day first, so a row Launchpad adds between two
  # pages shifts the next page by one, which then repeats a row: more rows
  # than the total, or a later page reporting another total, is a collection
  # that changed while it was paged.
  page <- function(day, total, start = 0L, next_link = NULL)
    counts_json("r-cran-a", "1", day, rep(1L, length(day)), next_link = next_link,
                total_size = total, start = start)
  pages <- list(
    short   = page(c("2026-09-03", "2026-09-02"), 3L, next_link = "short2"),
    short2  = page(character(0), 3L, start = 2L),            # empty page 2 of 3 rows
    whole   = page(c("2026-09-03", "2026-09-02"), 3L, next_link = "whole2"),
    whole2  = page("2026-09-01", 3L, start = 2L),
    none    = page(character(0), 0L),                         # nothing to count
    lost    = page(character(0), 5L),                         # empty, but 5 rows exist
    over    = page(c("2026-09-03", "2026-09-02"), 1L),        # more rows than it said
    # 09-04 arrived after page 1: page 2 starts a row earlier and repeats 09-02
    grew    = page(c("2026-09-03", "2026-09-02"), 3L, next_link = "grew2"),
    grew2   = page(c("2026-09-02", "2026-09-01"), 4L, start = 2L),
    # the same, with the last row of page 2 not reached: the rows match page 1's total
    moved   = page(c("2026-09-03", "2026-09-02"), 3L, next_link = "moved2"),
    moved2  = page("2026-09-02", 4L, start = 2L),
    untold  = '{"start":0,"entries":[{"binary_package_name":"r-cran-a",
                "binary_package_version":"1","day":"2026-09-01","count":2}]}')
  urls <- c("short", "whole", "none", "lost", "over", "grew", "moved", "untold")
  res <- fetch_paginated(function(u) lapply(u, function(x) pages[[x]]), urls, parse_counts_page, "rows")
  expect_identical(res$ok, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(nrow(res$data[[2]]), 3L)
  one <- function(u) pages[[u]]
  expect_error(paginate(one, "short", parse_counts_page, "rows"), "returned 2 rows, not the 3")
  expect_error(paginate(one, "lost", parse_counts_page, "rows"), "returned 0 rows, not the 5")
  expect_error(paginate(one, "over", parse_counts_page, "rows"), "returned 2 rows, not the 1")
  expect_error(paginate(one, "grew", parse_counts_page, "rows"), "changed size while it was paged")
  expect_error(paginate(one, "moved", parse_counts_page, "rows"), "changed size while it was paged")
  expect_identical(nrow(paginate(one, "whole", parse_counts_page, "rows")), 3L)
  expect_identical(nrow(paginate(one, "none", parse_counts_page, "rows")), 0L)
  expect_identical(nrow(paginate(one, "untold", parse_counts_page, "rows")), 1L)
})

test_that("with_retry makes five attempts by default, without promise warnings", {
  n <- 0L
  expect_no_warning(
    expect_error(with_retry({ n <- n + 1L; stop("HTTP 503") }, wait = 0), "HTTP 503"))
  expect_identical(n, 5L)
  n <- 0L
  expect_no_warning(got <- with_retry({ n <- n + 1L; if (n < 3L) stop("HTTP 503"); "body" }, wait = 0))
  expect_identical(got, "body")
  expect_identical(n, 3L)
})

test_that("with_retry re-raises a gone condition without retrying", {
  n <- 0L
  expect_error(
    with_retry({ n <- n + 1L; stop(errorCondition("HTTP 404", class = "c2d4u_gone")) }, wait = 0),
    class = "c2d4u_gone")
  expect_identical(n, 1L)
})

test_that("default_io fetch fails fast on HTTP 404 and 410", {
  for (code in c(404L, 410L)) {
    calls <- 0L
    local_mocked_bindings(
      curl_fetch_memory = function(url, handle = NULL) {
        calls <<- calls + 1L
        list(status_code = code, content = raw(0))
      },
      .package = "curl")
    expect_null(default_io()$fetch("https://example.invalid/gone"))
    expect_identical(calls, 1L)
  }
})

test_that("default_io fetch returns the body on HTTP 200", {
  local_mocked_bindings(
    curl_fetch_memory = function(url, handle = NULL) list(status_code = 200L, content = charToRaw("ok")),
    .package = "curl")
  expect_identical(default_io()$fetch("https://example.invalid/ok"), "ok")
})

test_that("default_io fetch_many passes its deadline to the pool", {
  local_mocked_bindings(
    multi_run = function(...) stop("multi_run must not be called past the deadline"),
    .package = "curl")
  out <- default_io()$fetch_many(c("u1", "u2"), deadline = Sys.time() - 1)
  expect_true(all(vapply(out, is.null, logical(1))))
})

test_that("the release counts as missing only when gh says it is not found", {
  calls <- 0L
  view <- function(status, output) function() {
    calls <<- calls + 1L
    list(status = status, output = output)
  }
  expect_true(gh_release_exists(view(0L, "title:\tc2d4u Downloads (rolling)"), wait = 0))
  expect_false(gh_release_exists(view(1L, "release not found"), wait = 0))
  expect_identical(calls, 2L)
})

test_that("any other gh failure is retried and then stops instead of reading as no release", {
  calls <- 0L
  auth <- function() {
    calls <<- calls + 1L
    list(status = 1L, output = 'non-200 OK status code: 401 Unauthorized body: "Bad credentials"')
  }
  expect_error(gh_release_exists(auth, tries = 3L, wait = 0),
               "could not tell whether the current release exists.*401 Unauthorized")
  expect_identical(calls, 3L)

  calls <- 0L
  flaky <- function() {
    calls <<- calls + 1L
    if (calls == 1L) list(status = 1L, output = "dial tcp: connect: connection refused")
    else list(status = 0L, output = character(0))
  }
  expect_true(gh_release_exists(flaky, wait = 0))
  expect_identical(calls, 2L)
})

test_that("default_io release_exists reads gh's exit status and output", {
  bin <- withr::local_tempdir()
  gh <- file.path(bin, "gh")
  writeLines(c("#!/bin/sh", 'echo "$FAKE_GH_OUT" >&2', 'exit "$FAKE_GH_RC"'), gh)
  Sys.chmod(gh, "0755")
  withr::local_path(bin, action = "prefix")
  withr::local_envvar(FAKE_GH_OUT = "release not found", FAKE_GH_RC = "1")
  expect_false(default_io()$release_exists())
  withr::local_envvar(FAKE_GH_OUT = "", FAKE_GH_RC = "0")
  expect_true(default_io()$release_exists())
})
