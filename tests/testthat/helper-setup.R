# Auto-sourced by testthat. During test_dir() the working directory is
# tests/testthat, so the repo root is two levels up.
.c2_root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(.c2_root, "scripts", "config.R"))
source(file.path(.c2_root, "scripts", "helpers.R"))

.c2_update <- file.path(.c2_root, "scripts", "update.R")
if (file.exists(.c2_update)) source(.c2_update)
.c2_backfill <- file.path(.c2_root, "scripts", "backfill.R")
if (file.exists(.c2_backfill)) source(.c2_backfill)

fixture_path <- function(...) {
  file.path(.c2_root, "tests", "testthat", "fixtures", ...)
}
