# These tests exercise the real robservatory loader against fixture DBs.
test_that("build_identity_maps reads canonical names and states from the ledger", {
  d <- withr::local_tempdir()
  dbs <- mk_ledger_dbs(d,
    cran   = c(mass = "MASS", oldpkg = "OldPkg"),
    bioc   = c(deseq2 = "DESeq2"),
    states = c(mass = "live", oldpkg = "archived", deseq2 = "live"))
  m <- build_identity_maps(dbs$cran, dbs$bioc)
  expect_equal(unname(m$name_map[["mass"]]), "MASS")
  expect_equal(unname(m$name_map[["deseq2"]]), "DESeq2")
  expect_equal(unname(m$state_map[["oldpkg"]]), "archived")
  expect_equal(unname(m$state_map[["deseq2"]]), "live")
  expect_gte(m$n_cran, 2L); expect_gte(m$n_bioc, 1L)
})

test_that("load_gated_maps returns the maps when both floors pass", {
  d <- withr::local_tempdir()
  dbs <- mk_ledger_dbs(d, cran = c(mass = "MASS"), bioc = c(deseq2 = "DESeq2"))
  io  <- list(identity_dbs = function() dbs)
  m <- load_gated_maps(io, live_floor = 1L, bioc_floor = 1L)
  expect_equal(unname(m$name_map[["mass"]]), "MASS")
})

test_that("load_gated_maps errors when the size gate fails", {
  d <- withr::local_tempdir()
  dbs <- mk_ledger_dbs(d, cran = c(mass = "MASS"), bioc = c(deseq2 = "DESeq2"))
  io  <- list(identity_dbs = function() dbs)
  expect_error(load_gated_maps(io, live_floor = 999999L, bioc_floor = 1L), "size gate")
})

test_that("empty_identity_maps is a total-miss resolver", {
  m <- empty_identity_maps()
  expect_length(m$name_map, 0L); expect_length(m$state_map, 0L)
  expect_identical(m$n_cran, 0L); expect_identical(m$n_bioc, 0L)
})
