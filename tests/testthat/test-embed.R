test_that("embed_aux writes summary + roster and load_releases reads them", {
  p <- withr::local_tempfile(fileext = ".db")
  export_shard(p, data.frame(package="a", date="2026-01-01", count=1L))
  s <- empty_summary()
  s[1, ] <- list("a","a","cran","A",1L,1L,1L,1L,1L,1L,0.03,NA,"2026-01-01","2026-01-01",1L,"live")
  rel <- data.frame(archive="c2d4u4.0+", binary_name="r-cran-a", version="v", pub_id=1L,
                    package="a", origin="cran", canonical_name="A", identity_state="live",
                    cnt_total=1L, last_day="2026-01-01", done=1L, stringsAsFactors = FALSE)
  embed_aux(p, s, rel)
  got <- load_releases(p)
  expect_identical(got$package, "a")
  expect_identical(got$identity_state, "live")
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  expect_true(SUMMARY_TABLE %in% DBI::dbListTables(con))
})

test_that("load_releases returns typed empty frame when table absent", {
  p <- withr::local_tempfile(fileext = ".db")
  export_shard(p, data.frame(package="a", date="2026-01-01", count=1L))
  got <- load_releases(p)
  expect_identical(nrow(got), 0L)
  expect_true(all(c("archive","binary_name","version","pub_id","package",
                    "origin","canonical_name","identity_state","cnt_total","last_day","done")
                  %in% names(got)))
})
