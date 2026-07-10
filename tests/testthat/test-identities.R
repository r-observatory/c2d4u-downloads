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
