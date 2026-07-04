test_that("paginate follows next_link and stops on NULL fetch", {
  page1 <- paste(readLines(fixture_path("published_page.json"), warn = FALSE), collapse = "\n")
  page2 <- '{"start":1,"total_size":2,"entries":[{"self_link":".../+binarypub/999","binary_package_name":"r-cran-zoo","binary_package_version":"1.8","source_package_name":"zoo","distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64","status":"Published","date_published":"2023-01-01T00:00:00+00:00"}]}'
  seen <- 0L
  fetch <- function(url) { seen <<- seen + 1L; if (seen == 1L) page1 else page2 }
  df <- paginate(fetch, "URL1", parse_published_page, "entries")
  expect_identical(nrow(df), 3L)
  expect_true("r-cran-zoo" %in% df$binary_name)

  expect_error(paginate(function(u) NULL, "URL", parse_counts_page, "rows"), "fetch failed")
})

test_that("dedup_releases keeps one amd64 pub per name+version", {
  ent <- data.frame(archive = "c2d4u4.0+",
    pub_id = c(1L, 2L, 3L), binary_name = c("r-cran-a","r-cran-a","r-cran-a"),
    version = c("v1","v1","v2"), arch = c("s390x","amd64","amd64"),
    status = "Published", date_published = "2023-01-01", stringsAsFactors = FALSE)
  d <- dedup_releases(ent)
  expect_identical(nrow(d), 2L)
  expect_identical(d$pub_id[d$version == "v1"], 2L)  # amd64 preferred
})

test_that("build_roster resolves identities and drops toolchain", {
  ent <- data.frame(archive = "c2d4u4.0+",
    pub_id = c(1L, 2L), binary_name = c("r-cran-ggplot2","gsl-bin"),
    version = c("3.4.4","2.5"), arch = "amd64",
    status = "Published", date_published = "2023-01-01", stringsAsFactors = FALSE)
  r <- build_roster(ent, build_cran_map("ggplot2"), NULL)
  expect_identical(nrow(r), 1L)
  expect_identical(r$package, "ggplot2")
  expect_identical(r$done, 0L)
  expect_true(is.na(r$last_day))
})
