# local-bootstrap.R builds locally but never publishes: its output has no
# fetch records, no summed-history contract and covers one archive, so only
# backfill.yml may upload to `current`.

# Run the script in a scratch directory with every proxy pointed at a closed
# port, so a mode that reaches for the network fails at once instead of
# querying CRAN, Bioconductor, GitHub or Launchpad.
run_local_bootstrap <- function(...) {
  script <- file.path(.c2_root, "scripts", "local-bootstrap.R")
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  dead <- "http://127.0.0.1:9"
  env <- c(sprintf("%s=%s", c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY",
                              "ALL_PROXY"), dead), "no_proxy=", "NO_PROXY=", "GH_TOKEN=")
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), ...),
                                  stdout = TRUE, stderr = TRUE, env = env, timeout = 120))
  list(output = out, status = attr(out, "status") %||% 0L, files = list.files(wd))
}

test_that("local-bootstrap refuses its publish mode before doing anything", {
  res <- run_local_bootstrap("run")
  expect_false(identical(as.integer(res$status), 0L))
  expect_true(any(grepl("publishing is done by backfill.yml", res$output, fixed = TRUE)))
  expect_length(res$files, 0L)   # no state or output directory was even created
})

test_that("local-bootstrap names its local modes in the usage error", {
  res <- run_local_bootstrap("bogus")
  expect_false(identical(as.integer(res$status), 0L))
  expect_true(any(grepl("usage: local-bootstrap.R <validate|build> [limit]", res$output, fixed = TRUE)))
})
