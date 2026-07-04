test_that(".build_name_map keys by lowercase, first canonical wins", {
  m <- .build_name_map(c("MASS", "Matrix", "ggplot2"))
  expect_identical(unname(m[["mass"]]), "MASS")
  expect_identical(unname(m[["matrix"]]), "Matrix")
})

test_that("parse_views_packages extracts Package fields", {
  txt <- "Package: limma\nVersion: 1.0\n\nPackage: DESeq2\nVersion: 2.0\n"
  expect_identical(parse_views_packages(txt), c("limma", "DESeq2"))
})

test_that("resolve_identities routes prefixes and restores case", {
  cran <- build_cran_map(c("ggplot2", "MASS", "data.table"))
  bioc <- build_bioc_map(c("Biobase", "S4Vectors"))
  out <- resolve_identities(
    c("r-cran-ggplot2", "r-cran-mass", "r-cran-data.table",
      "r-bioc-biobase", "r-other-amsmercury", "gsl-bin", "littler"),
    cran, bioc)
  # toolchain debs dropped
  expect_identical(nrow(out), 5L)
  expect_identical(out$origin,
    c("cran", "cran", "cran", "bioc", "other"))
  expect_identical(out$package,
    c("ggplot2", "mass", "data.table", "biobase", "amsmercury"))
  expect_identical(out$canonical_name,
    c("ggplot2", "MASS", "data.table", "Biobase", NA_character_))
})

test_that("resolve_identities keeps origin and falls back to token when unmapped", {
  out <- resolve_identities("r-cran-archivedpkg", build_cran_map(character(0)), NULL)
  expect_identical(out$origin, "cran")
  expect_identical(out$canonical_name, "archivedpkg")
})

test_that("resolve_identities dedupes a token to one origin (cran wins)", {
  out <- resolve_identities(c("r-cran-foo", "r-bioc-foo"),
                            build_cran_map("foo"), build_bioc_map("foo"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$origin, "cran")
})
