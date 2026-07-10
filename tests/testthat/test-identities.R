test_that("parse_views_packages extracts Package fields", {
  txt <- "Package: limma\nVersion: 1.0\n\nPackage: DESeq2\nVersion: 2.0\n"
  expect_identical(parse_views_packages(txt), c("limma", "DESeq2"))
})

test_that("resolve_identities routes prefixes, restores case, carries identity_state", {
  maps <- mk_maps(
    name  = c(ggplot2 = "ggplot2", mass = "MASS", data.table = "data.table",
              biobase = "Biobase"),
    state = c(ggplot2 = "live", mass = "archived", data.table = "live",
              biobase = "live"))
  out <- resolve_identities(
    c("r-cran-ggplot2", "r-cran-mass", "r-cran-data.table",
      "r-bioc-biobase", "r-other-amsmercury", "gsl-bin", "littler"), maps)
  expect_identical(nrow(out), 5L)                      # toolchain debs dropped
  expect_identical(out$origin, c("cran", "cran", "cran", "bioc", "other"))
  expect_identical(out$package, c("ggplot2", "mass", "data.table", "biobase", "amsmercury"))
  expect_identical(out$canonical_name, c("ggplot2", "MASS", "data.table", "Biobase", NA_character_))
  expect_identical(out$identity_state, c("live", "archived", "live", "live", NA_character_))
})

test_that("resolve_identities falls back to token and NA state when absent from the ledger", {
  out <- resolve_identities("r-cran-archivedpkg", mk_maps())
  expect_identical(out$origin, "cran")
  expect_identical(out$canonical_name, "archivedpkg")   # token fallback preserved
  expect_identical(out$identity_state, NA_character_)   # honest unknown
})

test_that("resolve_identities keeps origin='other' off the leaderboard (canonical + state NA)", {
  out <- resolve_identities("r-other-nitpick",
                            mk_maps(c(nitpick = "Nitpick"), c(nitpick = "live")))
  expect_identical(out$origin, "other")
  expect_identical(out$canonical_name, NA_character_)
  expect_identical(out$identity_state, NA_character_)
})

test_that("resolve_identities dedupes a token to one origin (cran wins)", {
  out <- resolve_identities(c("r-cran-foo", "r-bioc-foo"),
                            mk_maps(c(foo = "Foo"), c(foo = "live")))
  expect_identical(nrow(out), 1L)
  expect_identical(out$origin, "cran")
})

test_that("build_roster carries origin, canonical_name, and identity_state", {
  ent <- data.frame(
    archive = "c2d4u4.0+", binary_name = "r-cran-mass", version = "7.3", pub_id = 5L,
    arch = "amd64", status = "Published", date_published = "2023-01-01T00:00:00+00:00",
    stringsAsFactors = FALSE)
  r <- build_roster(ent, mk_maps(c(mass = "MASS"), c(mass = "archived")))
  expect_identical(r$package, "mass")
  expect_identical(r$canonical_name, "MASS")
  expect_identical(r$identity_state, "archived")
  expect_true("identity_state" %in% names(r))
})

# Old-schema releases table: the column set before identity_state was added
# (releases_table_ddl minus the identity_state line), built directly so the
# migration branch in load_releases is exercised on a table that predates the
# column.
.old_schema_releases_ddl <- function(table) sprintf(
  "CREATE TABLE %s (
     archive        TEXT,
     binary_name    TEXT,
     version        TEXT,
     pub_id         INTEGER,
     package        TEXT,
     origin         TEXT,
     canonical_name TEXT,
     cnt_total      INTEGER,
     last_day       TEXT,
     done           INTEGER,
     PRIMARY KEY (archive, binary_name, version))", table)

test_that("load_releases migrates a zero-row old-schema table without erroring", {
  p <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, .old_schema_releases_ddl(RELEASES_TABLE))
  DBI::dbDisconnect(con)

  got <- load_releases(p)
  expect_identical(nrow(got), 0L)
  expect_true("identity_state" %in% names(got))
  expect_identical(got$identity_state, character(0))
})

test_that("load_releases backfills identity_state as NA for an old-schema row", {
  p <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, .old_schema_releases_ddl(RELEASES_TABLE))
  DBI::dbWriteTable(con, RELEASES_TABLE, data.frame(
    archive = "c2d4u4.0+", binary_name = "r-cran-a", version = "v", pub_id = 1L,
    package = "a", origin = "cran", canonical_name = "A",
    cnt_total = 1L, last_day = "2026-01-01", done = 1L,
    stringsAsFactors = FALSE), append = TRUE)
  DBI::dbDisconnect(con)

  got <- load_releases(p)
  expect_identical(nrow(got), 1L)
  expect_true("identity_state" %in% names(got))
  expect_identical(got$identity_state, NA_character_)
})
