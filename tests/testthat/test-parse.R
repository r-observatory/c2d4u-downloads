test_that("lp_archive_ref encodes the plus and owner", {
  a <- ARCHIVES[[1]]
  expect_identical(lp_archive_ref(a),
    "https://api.launchpad.net/1.0/~c2d4u.team/+archive/ubuntu/c2d4u4.0%2B")
  expect_identical(lp_archive_ref(ARCHIVES[[2]]),
    "https://api.launchpad.net/1.0/~marutter/+archive/ubuntu/c2d4u3.5")
})

test_that("lp_published_url builds a filtered, paged enumeration URL", {
  u <- lp_published_url(ARCHIVES[[1]], start = 300L, size = 300L, status = "Published")
  expect_match(u, "ws.op=getPublishedBinaries")
  expect_match(u, "ordered=false")  # required to page deep without 503s
  expect_match(u, "ws.size=300")
  expect_match(u, "ws.start=300")
  expect_match(u, "status=Published")
  expect_match(u, "c2d4u4.0%2B")
})

test_that("lp_counts_url targets a binarypub and honours start_date", {
  u <- lp_counts_url(ARCHIVES[[1]], 198161808L, start_date = "2026-01-01")
  expect_match(u, "/\\+binarypub/198161808\\?")
  expect_match(u, "ws.op=getDownloadCounts")
  expect_match(u, "start_date=2026-01-01")
  expect_false(grepl("start_date", lp_counts_url(ARCHIVES[[1]], 1L)))
})

test_that("lp_pub_id and parse_arch extract trailing path segments", {
  expect_identical(lp_pub_id(
    "https://api.launchpad.net/1.0/~c2d4u.team/+archive/ubuntu/c2d4u4.0+/+binarypub/198161808"),
    198161808L)
  expect_identical(parse_arch("https://api.launchpad.net/1.0/ubuntu/jammy/amd64"), "amd64")
})

test_that("parse_published_page yields typed rows and the next link", {
  txt <- readLines(fixture_path("published_page.json"), warn = FALSE)
  p <- parse_published_page(paste(txt, collapse = "\n"))
  expect_identical(nrow(p$entries), 2L)
  expect_identical(p$entries$pub_id[1], 198161808L)
  expect_identical(p$entries$binary_name[1], "r-cran-ggplot2")
  expect_identical(p$entries$arch, c("amd64", "s390x"))
  expect_match(p$next_link, "ws.start=1")
})

test_that("parse_published_page returns NA next_link when absent", {
  p <- parse_published_page('{"start":0,"total_size":0,"entries":[]}')
  expect_identical(nrow(p$entries), 0L)
  expect_true(is.na(p$next_link))
})

test_that("parse_counts_page yields day/count rows", {
  txt <- readLines(fixture_path("counts_page.json"), warn = FALSE)
  p <- parse_counts_page(paste(txt, collapse = "\n"))
  expect_identical(nrow(p$rows), 3L)
  expect_identical(p$rows$day, c("2026-05-04", "2026-05-01", "2026-04-23"))
  expect_identical(p$rows$count, c(12L, 7L, 124L))
  expect_true(is.na(p$next_link))
})
