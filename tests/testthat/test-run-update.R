# A `pub` temp dir stands in for the persisted GitHub release.
fake_io <- function(pub, pages = list(), cran = character(0), bioc = character(0),
                    now = as.POSIXct("2026-07-04 00:00:00", tz = "UTC"),
                    ledger = NULL, fail_identity = FALSE) {
  list(
    release_exists = function() file.exists(file.path(pub, "manifest.json")),
    release_download = function(pattern, dir) {
      rx <- utils::glob2rx(pattern)
      hit <- list.files(pub, pattern = rx)
      if (length(hit) == 0) return(1L)
      for (h in hit) file.copy(file.path(pub, h), file.path(dir, h), overwrite = TRUE)
      0L
    },
    fetch = function(url) pages[[url]] %||% NULL,
    cran_names = function() cran,
    bioc_names = function() bioc,
    identity_dbs = function() {
      if (isTRUE(fail_identity) || is.null(ledger)) stop("identity asset unreachable (test)")
      ledger
    },
    now = function() now)
}
publish <- function(out, pub) {
  for (f in list.files(out, pattern = "\\.(db|json)$", full.names = TRUE))
    file.copy(f, file.path(pub, basename(f)), overwrite = TRUE)
}

test_that("cold run enumerates, fetches counts, and writes shards", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_published_url(a)]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":2,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2026-05-01","count":7},
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2026-05-02","count":9}]}'
  res <- run_update(fake_io(pub, pages, cran = "ggplot2"), out)
  expect_true(any(grepl("2026", res$changed_shards)))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-2026.db"))
  on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT SUM(count) s FROM c2d4u_downloads_daily")
  expect_identical(got$s, 16L)
})

test_that("protect-history aborts when recent shard cannot be downloaded", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  writeLines("{}", file.path(pub, "manifest.json"))  # release exists but no recent shard
  expect_error(run_update(fake_io(pub), out), "protect")
})

test_that("no active releases and no source yields a frozen heartbeat", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  # Seed a prior release: manifest + recent shard with a done, long-dead release.
  writeLines('{"source_kind":"launchpad","shards":{}}', file.path(pub, "manifest.json"))
  rp <- file.path(pub, "c2d4u-downloads-recent.db")
  export_shard(rp, data.frame(package="ggplot2", date="2020-01-01", count=1L))
  s <- empty_summary(); s[1,] <- list("ggplot2","ggplot2","cran","ggplot2",0L,0L,0L,1L,1L,1L,0,NA,"2020-01-01","2020-01-01",1L,"live")
  rel <- data.frame(archive="c2d4u4.0+", binary_name="r-cran-ggplot2", version="3.4.4", pub_id=10L,
                    package="ggplot2", origin="cran", canonical_name="ggplot2", identity_state="live",
                    cnt_total=1L, last_day="2020-01-01", done=1L, stringsAsFactors=FALSE)
  embed_aux(rp, s, rel)
  res <- run_update(fake_io(pub, pages = list(), cran = "ggplot2"), out)
  expect_identical(res$changed_shards, character(0))
  expect_identical(res$manifest$source_kind, "frozen")
})

test_that("a partial year-shard download does not drop earlier history from a touched year", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  a <- ARCHIVES[[1]]
  writeLines('{"source_kind":"launchpad","shards":{}}', file.path(pub, "manifest.json"))
  rp <- file.path(pub, "c2d4u-downloads-recent.db")
  export_shard(rp, data.frame(package = "ggplot2", date = "2026-03-01", count = 5L,
                              stringsAsFactors = FALSE))
  s <- empty_summary()
  s[1, ] <- list("ggplot2","ggplot2","cran","ggplot2",5L,5L,5L,1L,1L,1L,0.17,NA,"2026-03-01","2026-03-01",5L,"live")
  rel <- data.frame(archive = "c2d4u4.0+", binary_name = "r-cran-ggplot2", version = "3.4.4",
                    pub_id = 10L, package = "ggplot2", origin = "cran", canonical_name = "ggplot2",
                    identity_state = "live",
                    cnt_total = 5L, last_day = "2026-03-01", done = 1L, stringsAsFactors = FALSE)
  embed_aux(rp, s, rel)
  # A 2025 year shard is present but the 2026 year shard is (simulated) missing.
  export_shard(file.path(pub, "c2d4u-downloads-2025.db"),
               data.frame(package = "ggplot2", date = "2025-06-01", count = 2L, stringsAsFactors = FALSE))
  sd <- format(as.Date("2026-03-01") - REVISION_WINDOW_DAYS, "%Y-%m-%d")
  pages <- list()
  pages[[lp_counts_url(a, 10L, start_date = sd)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2026-06-15","count":3}]}'
  res <- run_update(fake_io(pub, pages, cran = "ggplot2"), out)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-2026.db"))
  on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT date, count FROM c2d4u_downloads_daily ORDER BY date")
  expect_true("2026-03-01" %in% got$date)  # earlier-in-year history preserved via the recent floor
  expect_true("2026-06-15" %in% got$date)  # new data present
})

test_that("cold run enriches canonical_name and identity_state from the ledger", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(),
    cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"),
    states = c(ggplot2 = "archived", biobase = "live"))
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_published_url(a)]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2026-05-01","count":7}]}'
  res <- run_update(fake_io(pub, pages, ledger = led), out, live_floor = 1L, bioc_floor = 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package, canonical_name, identity_state FROM c2d4u_downloads_summary")
  expect_identical(s$canonical_name, "ggplot2")
  expect_identical(s$identity_state, "archived")
})

test_that("cold run degrades honestly when the ledger is unreachable (token canonical, NA state)", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_published_url(a)]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ghostpkg","binary_package_version":"1.0",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ghostpkg","binary_package_version":"1.0","day":"2026-05-01","count":3}]}'
  res <- run_update(fake_io(pub, pages, fail_identity = TRUE), out, live_floor = 1L, bioc_floor = 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package, canonical_name, identity_state FROM c2d4u_downloads_summary")
  expect_identical(s$package, "ghostpkg")
  expect_identical(s$canonical_name, "ghostpkg")     # token fallback: row NOT dropped
  expect_true(is.na(s$identity_state))               # honest unknown
})

test_that("run_update degrades when the identity size gate fails", {
  pub <- withr::local_tempdir(); out <- withr::local_tempdir()
  led <- mk_ledger_dbs(withr::local_tempdir(), cran = c(ggplot2 = "ggplot2"), bioc = c(biobase = "Biobase"))
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_published_url(a)]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  pages[[lp_counts_url(a, 10L)]] <-
    '{"start":0,"total_size":1,"next_collection_link":null,"entries":[
      {"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4","day":"2026-05-01","count":7}]}'
  res <- run_update(fake_io(pub, pages, ledger = led), out, live_floor = 999999L, bioc_floor = 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "c2d4u-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT canonical_name, identity_state FROM c2d4u_downloads_summary")
  expect_identical(s$canonical_name, "ggplot2")      # token fallback (gate failed)
  expect_true(is.na(s$identity_state))
})
