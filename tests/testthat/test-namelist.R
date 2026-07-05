# Offline unit tests for the name-list enumeration primitives used by the
# sharded backfill: the candidate name universe, the CRAN Archive index parse,
# the concurrent per-name enumerate, and the even mod-N sharding.

test_that("candidate_binary_names unions CRAN + archive as r-cran-* and Bioc as r-bioc-*", {
  cand <- candidate_binary_names(
    cran_names    = c("ggplot2", "MASS"),
    archive_names = c("ggplot2", "oldpkg"),   # overlaps ggplot2, adds an archived-only name
    bioc_names    = c("Biobase"))
  expect_true(all(c("r-cran-ggplot2", "r-cran-mass", "r-cran-oldpkg", "r-bioc-biobase") %in% cand))
  # lowercased, deduplicated across the CRAN/archive overlap
  expect_identical(sum(cand == "r-cran-ggplot2"), 1L)
  expect_false(any(grepl("[A-Z]", cand)))
})

test_that("candidate_binary_names drops empty/NA and omits Bioc when none", {
  cand <- candidate_binary_names(c("a", "", NA_character_), character(0), character(0))
  expect_true("r-cran-a" %in% cand)
  expect_false(any(startsWith(cand, "r-bioc-")))
  # the fixed r-other- extras are always included
  expect_true(all(paste0("r-other-", c("amsmercury", "curvefdp", "hms-dbmi-spp",
                                        "iwrlars", "nitpick")) %in% cand))
})

test_that("parse_archive_index extracts directory names from the listing HTML", {
  html <- paste0(
    '<a href="../">../</a>',
    '<a href="ggplot2/">ggplot2/</a>',
    '<a href="A3/">A3/</a>',
    '<a href="README.html">README.html</a>')   # a file, not a dir: ignored
  nm <- parse_archive_index(html)
  expect_identical(nm, c("ggplot2", "A3"))
  expect_identical(parse_archive_index(NULL), character(0))
  expect_identical(parse_archive_index(""), character(0))
})

test_that("enumerate_names queries each candidate and tags rows with the archive", {
  a <- ARCHIVES[[1]]
  pages <- list()
  pages[[lp_name_query_url(a, "r-cran-ggplot2")]] <- paste0(
    '{"start":0,"total_size":1,"entries":[{"self_link":".../+binarypub/10",',
    '"binary_package_name":"r-cran-ggplot2","binary_package_version":"3.4.4",',
    '"distro_arch_series_link":"https://api.launchpad.net/1.0/ubuntu/jammy/amd64",',
    '"status":"Published","date_published":"2023-10-17T00:00:00+00:00"}]}')
  # r-cran-missing has no page -> fetch returns NULL (a 503/404), contributes nothing
  fetch_many <- function(urls) lapply(urls, function(u) pages[[u]] %||% NULL)
  ent <- enumerate_names(fetch_many, c("r-cran-ggplot2", "r-cran-missing"), a)
  expect_identical(nrow(ent), 1L)
  expect_identical(ent$binary_name, "r-cran-ggplot2")
  expect_identical(ent$pub_id, 10L)
  expect_identical(unique(ent$archive), a$key)
})

test_that("enumerate_names follows next_collection_link across pages", {
  a <- ARCHIVES[[1]]
  u1 <- lp_name_query_url(a, "r-cran-multi")
  u2 <- "https://api.launchpad.net/1.0/next-page-2"
  pages <- list()
  pages[[u1]] <- paste0(
    '{"start":0,"total_size":2,"next_collection_link":"', u2, '","entries":[',
    '{"self_link":".../+binarypub/1","binary_package_name":"r-cran-multi",',
    '"binary_package_version":"1.0","distro_arch_series_link":',
    '"https://api.launchpad.net/1.0/ubuntu/jammy/amd64","status":"Published",',
    '"date_published":"2023-01-01T00:00:00+00:00"}]}')
  pages[[u2]] <- paste0(
    '{"start":1,"total_size":2,"entries":[',
    '{"self_link":".../+binarypub/2","binary_package_name":"r-cran-multi",',
    '"binary_package_version":"2.0","distro_arch_series_link":',
    '"https://api.launchpad.net/1.0/ubuntu/jammy/amd64","status":"Published",',
    '"date_published":"2023-02-01T00:00:00+00:00"}]}')
  fetch_many <- function(urls) lapply(urls, function(u) pages[[u]] %||% NULL)
  ent <- enumerate_names(fetch_many, "r-cran-multi", a)
  expect_identical(nrow(ent), 2L)
  expect_identical(sort(ent$pub_id), c(1L, 2L))
})

test_that("enumerate_names on no candidates yields an empty archive-tagged frame", {
  ent <- enumerate_names(function(urls) list(), character(0), ARCHIVES[[1]])
  expect_identical(nrow(ent), 0L)
  expect_true("archive" %in% names(ent))
})

test_that("shard_rows partitions row indices evenly, disjointly, and completely", {
  n <- 100L; N <- 12L
  buckets <- lapply(0:(N - 1L), function(i) shard_rows(n, i, N))
  # complete cover with no overlap
  expect_identical(sort(unlist(buckets)), seq_len(n))
  expect_identical(sum(lengths(buckets)), n)
  # sizes differ by at most one (even split), never first-letter skew
  expect_lte(max(lengths(buckets)) - min(lengths(buckets)), 1L)
  # membership matches the modulo rule
  expect_identical(shard_rows(n, 0L, N), which(((seq_len(n) - 1L) %% N) == 0L))
  expect_identical(shard_rows(0L, 0L, N), integer(0))
})
