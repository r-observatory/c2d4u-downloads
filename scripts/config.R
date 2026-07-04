# Constants only. No functions, no side effects. Sourced first by everything.

LP_API_BASE  <- "https://api.launchpad.net/1.0"
PUBLISH_REPO <- "r-observatory/c2d4u-downloads"
USER_AGENT   <- "r-observatory-c2d4u-downloads/1.0 (+https://github.com/r-observatory/c2d4u-downloads)"

# Archive registry. The acquisition layer loops the enabled archives; the two
# older archives are enabled later without any other change.
ARCHIVES <- list(
  list(key = "c2d4u4.0+", owner = "c2d4u.team", ref = "c2d4u4.0+", r_era = "4.0", enabled = TRUE),
  list(key = "c2d4u3.5",  owner = "marutter",   ref = "c2d4u3.5",  r_era = "3.5", enabled = FALSE),
  list(key = "c2d4u",     owner = "marutter",   ref = "c2d4u",     r_era = "3.x", enabled = FALSE)
)

PAGE_SIZE            <- 300L   # Launchpad max ws.size
FETCH_POOL           <- 4L     # concurrent GET pool size (polite)
RECENT_WINDOW_DAYS   <- 400L   # rolling recent shard window
ACTIVE_WINDOW_DAYS   <- 180L   # a release is "active" (re-fetched monthly) if its last_day is within this
REVISION_WINDOW_DAYS <- 90L    # trailing days re-fetched each refresh to absorb Launchpad's ~60-day lag

CRAN_REPO             <- "https://cloud.r-project.org"
BIOC_VIEWS_BASE       <- "https://bioconductor.org/packages/release"
BIOC_VIEWS_CATEGORIES <- c("bioc", "data/annotation", "data/experiment", "workflows")
LOAD_BIOC_MAP         <- TRUE  # c2d4u ships r-bioc-* packages

SHARD_PREFIX   <- "c2d4u-downloads"
DAILY_TABLE    <- "c2d4u_downloads_daily"
SUMMARY_TABLE  <- "c2d4u_downloads_summary"
RELEASES_TABLE <- "c2d4u_releases"

# Fixed column order for the summary table (also the DDL order).
SUMMARY_COLS <- c(
  "package", "package_lower", "origin", "canonical_name",
  "total_30d", "total_90d", "total_365d",
  "rank_30d", "rank_90d", "rank_365d",
  "avg_daily_30d", "trend", "first_date", "last_date", "cnt_total"
)

# Env var used by the CLI entrypoints to force a full re-fetch/rebuild.
FORCE_REBUILD_ENV <- "C2D4U_FORCE_REBUILD"
