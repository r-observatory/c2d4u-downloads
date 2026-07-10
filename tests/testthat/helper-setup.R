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

# Build a pair of ledger fixture DBs (cran_names_all / bioc_names_all) for the
# robservatory identity loader. `cran`/`bioc` are named character vectors:
# name_lower -> canonical; `states` (optional) name_lower -> live|archived.
mk_ledger_dbs <- function(dir, cran = character(0), bioc = character(0),
                          states = character(0)) {
  st <- function(k) { s <- unname(states[k]); ifelse(is.na(s), "live", s) }
  write_one <- function(path, table, vals) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT,
         identity_state TEXT, first_seen TEXT, last_seen TEXT)", table))
    if (length(vals) > 0L)
      DBI::dbWriteTable(con, table, data.frame(
        name_lower = names(vals), canonical_name = unname(vals),
        identity_state = st(names(vals)), first_seen = "x", last_seen = "y",
        stringsAsFactors = FALSE), append = TRUE)
  }
  cp <- file.path(dir, "cran-archive.db"); bp <- file.path(dir, "bioc-meta.db")
  write_one(cp, "cran_names_all", cran)
  write_one(bp, "bioc_names_all", bioc)
  list(cran = cp, bioc = bp)
}

# A hand-built ledger maps list (bypasses robservatory) for resolver unit tests:
# `name` is name_lower -> canonical, `state` is name_lower -> live|archived.
mk_maps <- function(name = character(0), state = character(0),
                    n_cran = length(name), n_bioc = 0L) {
  list(name_map = name, state_map = state, n_cran = n_cran, n_bioc = n_bioc)
}
