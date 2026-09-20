# Constants only. No functions, no side effects. Sourced first by everything.

LP_API_BASE  <- "https://api.launchpad.net/1.0"
PUBLISH_REPO <- "r-observatory/c2d4u-downloads"
USER_AGENT   <- "r-observatory-c2d4u-downloads/1.0 (+https://github.com/r-observatory/c2d4u-downloads)"

# Archive registry. The acquisition layer loops the enabled archives; the two
# older archives are enabled later without any other change.
ARCHIVES <- list(
  list(key = "c2d4u4.0+", owner = "c2d4u.team", ref = "c2d4u4.0+", r_era = "4.0", enabled = TRUE),
  list(key = "c2d4u3.5",  owner = "marutter",   ref = "c2d4u3.5",  r_era = "3.5", enabled = TRUE),
  list(key = "c2d4u",     owner = "marutter",   ref = "c2d4u",     r_era = "3.x", enabled = TRUE)
)

PAGE_SIZE            <- 300L   # Launchpad max ws.size
FETCH_POOL           <- 4L     # concurrent GET pool size (polite)
# Concurrent fetch pool for the sharded backfill (fetch_pool). Launchpad
# throttles above ~16-24 aggregate connections, so the workflow keeps
# max-parallel * POOL <= ~24. A single lone job (enumerate) may raise it via
# the C2D4U_POOL env override; the local name-list run was reliable at 16.
POOL                 <- 6L     # concurrent connections per shard
FETCH_PASSES         <- 4L     # total attempts per url (1 + 3 retries) to ride out 503 waves
ENUM_BATCH           <- 300L   # candidate names per enumerate wave
# Both windows below are measured back from the data edge (the manifest's
# counted_through), never from the wall clock, so a frozen archive does not age
# out of the monthly update while its data stands still.
# INVARIANT: RECENT_WINDOW_DAYS must exceed ACTIVE_WINDOW_DAYS +
# REVISION_WINDOW_DAYS. The recent shard is anchored on the edge as well, so it
# holds the last recorded day of every release the update re-fetches (last_day
# >= edge - ACTIVE_WINDOW_DAYS) and every stored day the update compares against
# or rewrites (from edge - REVISION_WINDOW_DAYS), with more than a revision
# window of margin left beyond the activity window.
RECENT_WINDOW_DAYS   <- 400L   # rolling recent shard window
ACTIVE_WINDOW_DAYS   <- 365L   # a release is re-fetched monthly if its last_day is within this of the edge
REVISION_WINDOW_DAYS <- 30L    # days before the edge re-fetched each update to absorb late revisions
# Launchpad reports a day within a day of it ending, so only days at least this
# old are counted: counted_through is at most the fetch date minus this lag.
SETTLED_LAG_DAYS     <- 2L
# Releases per monthly-update fetch batch. Equal to fetch_pool's block so every
# batch runs as a single pool block (one slow tail per batch, not two).
UPDATE_BATCH         <- 1500L
# Minutes after run_update starts past which the mandatory window fetch stops
# the run and the optional full-history fetch ends early. It leaves room under
# the update step's 330-minute timeout for one bounded in-flight request, the
# rebuild and the export, so a slow run fails visibly instead of being
# cancelled inside the publish step.
DEADLINE_MIN         <- 280L
# The serial residual pass of the monthly update starts a url only with at
# least this many minutes left before the deadline: one url can take about 9
# minutes (see with_retry).
RESIDUAL_MIN_LEFT_MIN <- 10
# Minutes the monthly update's serial retry of the window releases may spend.
# A url that keeps failing costs at least 75 s of backoff, so without a bound a
# few dozen of them would use up the run; what the retry does not reach counts
# as failed and is held (within W_FAIL_MAX_FRAC) or stops the run as an outage.
WINDOW_RETRY_BUDGET_MIN <- 60
# How many dormant releases the monthly update probes for new downloads when no
# release is active any more (the ones downloaded last).
DORMANT_PROBE_N      <- 50L
# The viewer's loader reads the year shards named in the current manifest's
# changed_shards once a day. A monthly update published less than this many
# days after the previous publish keeps that publish's year entries listed, so
# they are loaded even if no load ran in between (a backfill followed by the
# queued monthly update). Longer ago, they have been loaded.
CHANGED_SHARDS_CARRY_DAYS <- 7
# Minutes a backfill fetch shard may spend retrying, one release at a time,
# what its concurrent pool could not fetch (CLI: C2D4U_RETRY_BUDGET_MIN). The
# pool leaves more behind as Launchpad throttles a long run: the 2026-09-20
# backfill left up to 2,913 releases in a shard, and the retry cleared about 36
# a minute, so 60 minutes ran out with 750 still unfetched and the merge refused
# to publish a shortfall.
RETRY_BUDGET_MIN     <- 150

# Date ranges where Launchpad itself recorded nothing for every publication, so
# a run of zero-download days inside one is a source hole, not a stall of ours.
# Both bounds inclusive. The update does not hold counted_through back for them.
KNOWN_SOURCE_GAPS <- list(
  list(from = "2026-05-06", to = "2026-07-11", note = "Launchpad recorded no downloads")
)
# A trailing run of at least this many ecosystem-wide zero days outside a known
# gap means Launchpad has stalled: counted_through stops before the run.
ZERO_RUN_DAYS <- 3L
# A stall ends once no release has had a download for more than this many days
# outside a known gap, counted from the last download anywhere: downloads have
# then stopped, and counted_through moves on through the quiet days, or it
# would stand still for good and every later run would publish nothing. Longer
# than the 67-day hole Launchpad left in 2026, which it never filled in.
STALL_MAX_DAYS <- 90L

# Monthly-update guards. REVISION_DROP_TOL is the largest relative drop allowed
# between the stored and the re-fetched overlap before the source counts as
# regressed; the per-package check applies only to packages with at least
# REGRESSION_MIN_STORED stored downloads in the overlap, and more than
# REGRESSION_MAX_PACKAGES regressed packages stop the run. More than
# W_FAIL_MAX_FRAC of the window releases failing is an outage and stops the run.
REVISION_DROP_TOL       <- 0.02
REGRESSION_MIN_STORED   <- 50L
REGRESSION_MAX_PACKAGES <- 10L
W_FAIL_MAX_FRAC         <- 0.02

# Backfill publish floor. An unfetched-release fraction above
# UNFETCHED_WARN_FRAC is published with a warning and above UNFETCHED_MAX_FRAC is
# refused. Against a published release, more than DOMINANCE_MAX_FRAC of packages
# counting fewer downloads than before stops the publish (a sum over every
# release can never be below the single-release count it replaces).
UNFETCHED_WARN_FRAC <- 0.01
UNFETCHED_MAX_FRAC  <- 0.10
DOMINANCE_MAX_FRAC  <- 0.01

# Release contract. Only a publisher that sums every release's downloads per
# (package, date) writes HISTORY_METHOD, and the monthly update refuses a release
# without it. The roster keeps one publication per (archive, binary, version),
# preferring amd64, so other architectures' downloads are out of scope.
HISTORY_METHOD <- "summed"
COVERAGE_SCOPE <- "amd64 publications only (one per archive, binary, version)"

# The file in a publisher's out dir naming, one per line, the database assets
# the run rebuilt. The workflows upload exactly these and then manifest.json.
# changed_shards is not that list: it names the year shards a loader should
# (re)read, which can include ones a recent publish already uploaded.
UPLOAD_LIST <- "upload-list.txt"

CRAN_REPO             <- "https://cloud.r-project.org"
CRAN_ARCHIVE_INDEX    <- "https://cran.r-project.org/src/contrib/Archive/"  # every ever-archived CRAN package
BIOC_VIEWS_BASE       <- "https://bioconductor.org/packages/release"
BIOC_VIEWS_CATEGORIES <- c("bioc", "data/annotation", "data/experiment", "workflows")

# Org identity ledger assets (canonical_name + identity_state source).
CRAN_ARCHIVE_REPO <- "r-observatory/cran-archive"
CRAN_ARCHIVE_DB   <- "cran-archive.db"
BIOC_META_REPO    <- "r-observatory/bioconductor-metadata"
BIOC_META_DB      <- "bioconductor-metadata.db"
CRAN_NAMES_FLOOR  <- 15000L   # below this the identity fetch is treated as partial
BIOC_NAMES_FLOOR  <- 1500L

SHARD_PREFIX   <- "c2d4u-downloads"
DAILY_TABLE    <- "c2d4u_downloads_daily"
SUMMARY_TABLE  <- "c2d4u_downloads_summary"
RELEASES_TABLE <- "c2d4u_releases"

# Fixed column order for the summary table (also the DDL order).
SUMMARY_COLS <- c(
  "package", "package_lower", "origin", "canonical_name",
  "total_30d", "total_90d", "total_365d",
  "rank_30d", "rank_90d", "rank_365d",
  "avg_daily_30d", "trend", "first_date", "last_date", "cnt_total",
  "identity_state"
)

# The monthly update's former full-rebuild switch. Its CLI now stops when it is
# set, because a full re-fetch and rebuild is done by backfill.yml.
FORCE_REBUILD_ENV <- "C2D4U_FORCE_REBUILD"
# Env var used by the CLI entrypoint to rebuild identity + summary from the
# already-downloaded shard history, with zero Launchpad calls.
RECLASSIFY_ONLY_ENV <- "C2D4U_RECLASSIFY_ONLY"
# Env var read by the update CLI for the fetch deadline in minutes (default
# DEADLINE_MIN).
DEADLINE_MIN_ENV <- "C2D4U_DEADLINE_MIN"
