# Monthly refresh for the c2d4u-downloads pipeline. The history itself is built
# by backfill.R (sharded) and published as a summed release: every release's
# downloads summed per (package, date) and counted through a settled day. The
# update only ever starts from such a release (its manifest says
# history_method "summed" and counted_through, and every asset matches its
# fingerprint), so it never builds on a partial, first-wins or torn history.
#
# Each run re-fetches, for every release downloaded within ACTIVE_WINDOW_DAYS
# of its package's edge, the days from REVISION_WINDOW_DAYS before that edge
# (or from an earlier refetch floor, so every newly counted day is fetched in
# two runs) through the last settled day, and replaces the package's stored
# days from there with the fetched sums, never lowering a day it had stored
# (Launchpad counts only grow). A package's edge is counted_through unless an
# earlier run held the package back (package_edges). Releases never fetched
# (done = 0), and releases whose one full fetch came back empty (done = 2,
# asked again before they count as never downloaded), are fetched in full;
# their days before the window (through the package's edge, for a package
# this run does not refresh) are added to the stored history, which never
# held them. All network and gh access is behind the injectable `io` so
# run_update runs fully offline in tests.

if (!exists("SHARD_PREFIX")) source(file.path("scripts", "config.R"))
if (!exists("build_roster")) source(file.path("scripts", "helpers.R"))

with_retry <- function(expr, tries = 5L, wait = 5) {
  # Exponential backoff (capped) so a short Launchpad 503 wave is ridden out.
  # Sleeps total 5+10+20+40 = 75 s over 5 tries; with the 90 s request timeout a
  # single url takes at most about 9 minutes, which bounds how far one serial
  # fetch can overrun a deadline checked before it starts. A condition of class
  # c2d4u_gone (HTTP 404/410: retrying cannot bring the resource back) is
  # re-raised at once. The expression is re-evaluated from its captured code on
  # every try (forcing the promise again would warn "restarting interrupted
  # promise evaluation").
  code <- substitute(expr); env <- parent.frame()
  for (i in seq_len(tries)) {
    val <- tryCatch(eval(code, env), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (inherits(val, "c2d4u_gone")) stop(val)
    if (i < tries) Sys.sleep(min(wait * 2^(i - 1), 120))
  }
  stop(val)
}

# Per-package data edge: the package's entry in `edges` (the published
# package_edges), else counted_through.
package_edge <- function(packages, counted_through, edges) {
  e <- rep(as.Date(counted_through), length(packages))
  m <- match(packages, names(edges))
  if (any(!is.na(m)))
    e[!is.na(m)] <- as.Date(unlist(edges, use.names = FALSE))[m[!is.na(m)]]
  e
}

# Per-package refetch floor: the earlier of floor_all (the published
# refetch_from, a day for every package, or NA) and the package's entry in
# `floors` (the published package_refetch_from); NA when neither applies.
package_floor <- function(packages, floor_all, floors) {
  f <- rep(as.Date(floor_all), length(packages))
  m <- match(packages, names(floors))
  if (any(!is.na(m))) {
    own <- as.Date(unlist(floors, use.names = FALSE))[m]
    f <- pmin(f, own, na.rm = TRUE)
  }
  f
}

# Classify the roster for one run. counted_through and edges come from the
# published manifest, settled is the last settled day T. Each release gets its
# package's edge E_p and window start S_p = E_p - REVISION_WINDOW_DAYS, or the
# package's refetch floor (floor_all, floors: see package_floor) when that is
# earlier, and falls in one class (row indices):
#   w        fetched before, last download on or after E_p - ACTIVE_WINDOW_DAYS
#            (or S_p, when a floor puts it further back, so every stored day
#            of the overlap [S_p, E_p] comes from a window release), and E_p
#            before T: re-fetched over [S_p, T]
#   f        not settled: never fetched (done = 0), then answered empty once
#            (done = 2, also listed as empty_once): fetched in full, in that
#            order, so a deadline leaves the releases most likely to have
#            downloads fetched first
#   never    settled with no downloads: answered empty twice
#   dormant  fetched before, last download older than the active window
#   waiting  active, but nothing is settled past its package's edge yet
# Every cut hangs off the edge, not the wall clock, so the classes stay the
# same while the data edge stands still.
update_plan <- function(roster, counted_through, edges, settled, floor_all = NA,
                        floors = list()) {
  settled <- as.Date(settled)
  edge  <- package_edge(roster$package, counted_through, edges)
  start <- pmin(edge - REVISION_WINDOW_DAYS, package_floor(roster$package, floor_all, floors),
                na.rm = TRUE)
  ld <- as.Date(roster$last_day)
  fetched <- !is.na(roster$done) & roster$done == 1L
  empty_once <- roster$done %in% 2L
  active  <- fetched & !is.na(ld) & ld >= pmin(edge - ACTIVE_WINDOW_DAYS, start)
  list(edge = edge, start = start,
       w       = which(active & edge < settled),
       f       = c(which(!fetched & !empty_once), which(empty_once)),
       empty_once = which(empty_once),
       never   = which(fetched & is.na(ld)),
       dormant = which(fetched & !is.na(ld) & !active),
       waiting = which(active & edge >= settled))
}

# One getDownloadCounts url per release in `rel`; start (a Date per release)
# and end (one Date) are omitted when NULL.
counts_urls <- function(rel, start = NULL, end = NULL) {
  if (nrow(rel) == 0L) return(character(0))
  vapply(seq_len(nrow(rel)), function(k)
    lp_counts_url(archive_by_key(rel$archive[k]), rel$pub_id[k],
                  start_date = if (!is.null(start)) format(start[k]),
                  end_date   = if (!is.null(end)) format(end)),
    character(1))
}

# Fetch one phase's urls (one per release): batches of `batch` through the pool
# (io$fetch_many), then a serial pass (io$fetch) over the releases the pool did
# not complete. A release counts only when every page came back, from the pool
# or from the serial pass; the pages the pool got before failing are thrown
# away, because the serial pass pages the release again from its first url.
# No batch starts past the deadline. No serial retry starts with less than
# RESIDUAL_MIN_LEFT_MIN minutes left, or once the pass has spent
# retry_budget_min minutes; the releases it does not reach stay failed, like
# the ones it retried in vain, so the caller's rule for failed releases decides
# what they mean rather than the clock. Returns ok, the rows of each ok release
# (NULL otherwise) and cut, TRUE when the deadline stopped the pool batches
# with releases not yet requested.
fetch_phase <- function(io, urls, phase, t0, deadline, batch, retry_budget_min = Inf) {
  n <- length(urls)
  ok <- logical(n); rows <- vector("list", n); cut <- FALSE
  secs_left <- function() as.numeric(deadline) - as.numeric(io$now())
  elapsed <- function() (as.numeric(io$now()) - as.numeric(t0)) / 60
  batches <- if (n) split(seq_len(n), ceiling(seq_len(n) / batch)) else list()
  for (k in seq_along(batches)) {
    if (secs_left() <= 0) { cut <- TRUE; break }
    idx <- batches[[k]]
    res <- fetch_paginated(io$fetch_many, urls[idx], parse_counts_page, "rows",
                           deadline = deadline, now = io$now)
    ok[idx] <- res$ok
    rows[idx[res$ok]] <- res$data[res$ok]
    message(sprintf("%s batch %d/%d: %d ok, %d failed, %.1f min elapsed",
                    phase, k, length(batches), sum(res$ok), sum(!res$ok), elapsed()))
  }
  retry <- which(!ok)
  if (cut || length(retry) == 0L) return(list(ok = ok, rows = rows, cut = cut))
  tried <- 0L; stopped <- NULL
  started <- as.numeric(io$now())
  for (k in retry) {
    if (secs_left() < RESIDUAL_MIN_LEFT_MIN * 60) { stopped <- "deadline"; break }
    if (as.numeric(io$now()) - started >= retry_budget_min * 60) {
      stopped <- "retry budget"; break
    }
    tried <- tried + 1L
    got <- tryCatch(paginate(io$fetch, urls[k], parse_counts_page, "rows"),
                    error = function(e) NULL)
    if (!is.null(got)) { ok[k] <- TRUE; rows[[k]] <- got }
  }
  message(sprintf("%s retry: %d of %d recovered serially, %d still failed%s, %.1f min elapsed",
                  phase, sum(ok[retry]), tried, tried - sum(ok[retry]),
                  if (!is.null(stopped))
                    sprintf(", %d not retried (%s)", length(retry) - tried, stopped) else "",
                  elapsed()))
  list(ok = ok, rows = rows, cut = cut)
}

# Row-bind per-release count frames (binary_name, version, day, count) column
# by column, which stays fast for tens of thousands of releases.
bind_counts <- function(lst) {
  lst <- Filter(function(r) !is.null(r) && nrow(r) > 0L, lst)
  if (length(lst) == 0L)
    return(data.frame(binary_name = character(0), version = character(0), day = character(0),
                      count = integer(0), stringsAsFactors = FALSE))
  col <- function(k) unlist(lapply(lst, `[[`, k), use.names = FALSE)
  data.frame(binary_name = col("binary_name"), version = col("version"), day = col("day"),
             count = as.integer(col("count")), stringsAsFactors = FALSE)
}

# Sum package-day rows that share a (package, date).
sum_daily <- function(df) {
  df <- df[c("package", "date", "count")]
  if (nrow(df) == 0L) return(df)
  key <- paste(df$package, df$date, sep = "\r")
  s <- rowsum(as.numeric(df$count), key, reorder = FALSE)
  first <- !duplicated(key)
  data.frame(package = df$package[first], date = df$date[first],
             count = as.integer(s[, 1]), stringsAsFactors = FALSE)
}

# Package-day rows of a and b with the larger count where both have the same
# (package, date).
max_daily <- function(a, b) {
  d <- rbind(a[c("package", "date", "count")], b[c("package", "date", "count")])
  if (nrow(d) == 0L) return(d)
  d <- d[order(d$package, d$date, -d$count), , drop = FALSE]
  d <- d[!duplicated(paste(d$package, d$date, sep = "\r")), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Add package-day rows onto a history: summed where the (package, date) is
# already stored, appended where it is not.
add_onto <- function(daily, add) {
  if (nrow(add) == 0L) return(daily)
  sel <- which(daily$package %in% add$package)
  m <- match(paste(add$package, add$date, sep = "\r"),
             paste(daily$package[sel], daily$date[sel], sep = "\r"))
  hit <- !is.na(m)
  daily$count[sel[m[hit]]] <- daily$count[sel[m[hit]]] + add$count[hit]
  rbind(daily, add[!hit, c("package", "date", "count"), drop = FALSE])
}

# Total count per package in pkgs over its own [from, to] (character dates
# aligned with pkgs); 0 for a package with no rows there.
overlap_sums <- function(daily, pkgs, from, to) {
  out <- stats::setNames(numeric(length(pkgs)), pkgs)
  if (length(pkgs) == 0L || nrow(daily) == 0L) return(out)
  m <- match(daily$package, pkgs)
  sel <- !is.na(m)
  sel[sel] <- daily$date[sel] >= from[m[sel]] & daily$date[sel] <= to[m[sel]]
  if (any(sel)) {
    s <- rowsum(as.numeric(daily$count[sel]), daily$package[sel])
    out[rownames(s)] <- s[, 1]
  }
  out
}

# Zero-download runs in the window (from, to]. `totals` is the ecosystem-wide
# download total by day (named by YYYY-MM-DD; a day without an entry is zero).
# A day inside a `known` source gap is explained and never counts toward a
# run. When the days reaching `to` are a zero run with at least min_run
# unexplained days, Launchpad has not reported them: t_eff stops before the
# first unexplained day of that run and `trailing` names it. Earlier runs of at
# least min_run unexplained days, after which downloads resumed, come back in
# `detected` as {from, to}.
#
# A stall cannot outlast max_run unexplained days, counted from the day after
# the last download: in the window, or before it (`last_seen`, the last day
# with a download on or before `from`) when the whole window is quiet. Past
# that, downloads have stopped: t_eff stays `to`, and `stopped` names the whole
# quiet stretch, which joins `detected` with kind "stopped". It is recorded
# from its first quiet day even when that is before the window: days a run
# already counted as quiet (a tail too short to be a stall) belong to it, and
# the next run fetches the stretch again from where it is recorded.
window_gaps <- function(totals, from, to, known = KNOWN_SOURCE_GAPS, min_run = ZERO_RUN_DAYS,
                        last_seen = NA, max_run = STALL_MAX_DAYS) {
  from <- as.Date(from); to <- as.Date(to)
  none <- list(t_eff = to, detected = list(), trailing = NULL, stopped = NULL)
  if (to <= from) return(none)
  in_known <- function(d) {
    e <- rep(FALSE, length(d))
    for (g in known) e <- e | (d >= as.Date(g$from) & d <= as.Date(g$to))
    e
  }
  days <- seq(from + 1L, to, by = "day"); dc <- format(days)
  tot <- unname(totals[dc]); tot[is.na(tot)] <- 0
  explained <- in_known(days)
  zero <- tot == 0
  n <- length(days); last <- n
  while (last >= 1L && zero[last]) last <- last - 1L
  out <- none
  if (last < n) {
    run <- (last + 1L):n
    un <- run[!explained[run]]
    if (length(un) >= min_run) {
      quiet_from <- if (last >= 1L) days[last] + 1
                    else if (!is.na(last_seen) && as.Date(last_seen) < from) as.Date(last_seen) + 1
                    else from + 1
      if (sum(!in_known(seq(quiet_from, to, by = "day"))) > max_run) {
        out$stopped <- list(from = format(quiet_from), to = dc[n])
      } else {
        out$t_eff <- as.Date(dc[min(un)]) - 1
        out$trailing <- list(from = dc[min(un)], to = dc[n])
      }
    }
  }
  if (last >= 1L) {
    r <- rle(zero[seq_len(last)] & !explained[seq_len(last)])
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
    for (k in which(r$values & r$lengths >= min_run))
      out$detected[[length(out$detected) + 1L]] <- list(from = dc[starts[k]], to = dc[ends[k]])
  }
  if (!is.null(out$stopped))
    out$detected[[length(out$detected) + 1L]] <- c(out$stopped, kind = "stopped")
  out
}

# Record newly detected gaps after the recorded ones. An exact repeat is
# dropped, and one that starts the day after the last recorded one ends
# extends it, so a stretch counted as stopped over several runs stays one
# entry. The extended entry takes the new part's kind: it is stopped while
# its latest part is, and an ordinary gap once downloads came back after it.
add_gaps <- function(old, new) {
  out <- old
  for (g in new) {
    k <- length(out)
    if (k > 0L && identical(format(as.Date(out[[k]]$to) + 1), g$from)) {
      out[[k]]$to <- g$to
      out[[k]]$kind <- g$kind           # NULL drops it
    } else if (!any(vapply(out, function(o) identical(o$from, g$from) && identical(o$to, g$to),
                           logical(1)))) {
      out[[k + 1L]] <- g
    }
  }
  out
}

# The full stored history: every downloaded year shard, with the recent shard
# as a floor (a recent row no year shard holds is kept). Only the year rows the
# recent shard can overlap are keyed, which keeps this cheap on the ~6M-row
# history.
load_history <- function(out_dir, recent_path) {
  year_files <- list.files(out_dir, full.names = TRUE,
    pattern = sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX))
  h <- do.call(rbind, c(list(load_daily(tempfile())), lapply(year_files, load_daily)))
  rec <- load_daily(recent_path)
  if (nrow(rec) == 0L) return(h)
  near <- which(h$date >= min(rec$date))
  have <- paste(rec$package, rec$date, sep = "\r") %in%
          paste(h$package[near], h$date[near], sep = "\r")
  rbind(h, rec[!have, , drop = FALSE])
}

# Refresh canonical_name / identity_state on every roster row from the ledger
# (NULL on degrade: persisted identity is then kept as it is).
enrich_roster <- function(roster, ledger) {
  if (is.null(ledger) || nrow(roster) == 0L) return(roster)
  ident <- resolve_identities(roster$binary_name, ledger)
  ib <- match(roster$binary_name, ident$binary_name)
  ok <- !is.na(ib)
  roster$origin[ok]         <- ident$origin[ib[ok]]
  roster$canonical_name[ok] <- ident$canonical_name[ib[ok]]
  roster$identity_state[ok] <- ident$identity_state[ib[ok]]
  roster
}

# Rebuild the summary over `daily` (the full history) anchored on `anchor`
# (and each package in `edges`, the package_edges being published, on its own
# edge), then export the year shards in `years`, the recent shard (anchored on
# `anchor`, carrying the summary and the roster) and the summary DB into
# out_dir. Returns the exported file names (years first) and the summary.
export_release <- function(out_dir, daily, roster, anchor, prior_summary, years, edges) {
  recent_path  <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(daily) > 0L)
    DBI::dbWriteTable(con, DAILY_TABLE, daily[c("package", "date", "count")], append = TRUE)
  summary_df <- build_summary(con, roster, anchor, prior_summary = prior_summary, edges = edges)
  files <- character(0)
  for (yr in years) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    export_shard(file.path(out_dir, f), extract_year(con, as.integer(yr)))
    files <- c(files, f)
  }
  export_shard(recent_path, extract_recent(con, anchor, RECENT_WINDOW_DAYS))
  embed_aux(recent_path, summary_df, roster)
  export_summary_shard(summary_path, summary_df)
  list(files = c(files, basename(recent_path), basename(summary_path)), summary = summary_df)
}

enabled_archive_keys <- function()
  vapply(Filter(function(a) isTRUE(a$enabled), ARCHIVES), function(a) a$key, character(1))

# changed_shards for a publish that rebuilt `files` (years first, then the
# recent and summary shards). A loader reads the year shards the CURRENT
# manifest names, once a day, so a publish that replaces one it may not have
# read yet keeps that one's year entries too: when the previous publish was
# less than carry_days before t0 (Inf: however long ago). Only year shards
# downloaded here, and so verified against the previous manifest, are kept.
changed_with_previous <- function(prev, out_dir, files, t0, carry_days = Inf) {
  year_rx <- sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX)
  last <- as.POSIXct(as.character(prev$last_changed %||% NA),
                     format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  recent <- is.infinite(carry_days) ||
    (!is.na(last) && as.numeric(difftime(t0, last, units = "days")) < carry_days)
  carried <- if (recent) as.character(unlist(prev$changed_shards)) else character(0)
  carried <- carried[grepl(year_rx, carried) & file.exists(file.path(out_dir, carried))]
  c(sort(unique(c(carried, files[grepl(year_rx, files)]))), files[!grepl(year_rx, files)])
}

# The refetch floors the next run starts from. Its window starts
# REVISION_WINDOW_DAYS before each package's edge, so when a refreshed
# package's new days (E_p, T_eff] run longer than REVISION_WINDOW_DAYS + 1,
# the earliest of them would be fetched only this once, and a short answer on
# one of them (a revision Launchpad had not made yet) would never be repaired
# by the max(stored, fetched) rule. Such a package gets the floor E_p + 1: the
# next run's window starts there. A package with a floor this run that was not
# refreshed (held, or with nothing settled past its edge; `active` names the
# packages with a window or waiting release) keeps it for the run that does.
# That run may be the first to store the days the floor covers: when
# Launchpad filled in days of a stopped stretch while the package was held,
# the run that found them stored nothing for it. So a package the published
# release held (`held_before`, its package_edges) keeps the floor it is
# refreshed from, when that reaches back past its revision window, for one
# more run.
# A stretch still counted as stopped after this run (`stopped_from`, its first
# day) is a floor for every package. Floors are sparse: `all` is one floor for
# every package, set by that stretch or when the packages at counted_through
# (which move together) ran long, and `packages` lists only the packages whose
# floor is earlier than that.
next_floors <- function(pkg_edge, pkg_floor, refreshed, active, ct_prev, t_eff,
                        stopped_from = as.Date(NA), held_before = character(0)) {
  nf <- stats::setNames(rep(as.Date(NA), length(pkg_edge)), names(pkg_edge))
  long <- refreshed[as.numeric(t_eff - pkg_edge[refreshed]) > REVISION_WINDOW_DAYS + 1]
  nf[long] <- pkg_edge[long] + 1
  again <- intersect(refreshed, held_before)
  again <- again[!is.na(pkg_floor[again]) &
                 pkg_floor[again] < pkg_edge[again] - REVISION_WINDOW_DAYS]
  nf[again] <- pmin(nf[again], pkg_floor[again], na.rm = TRUE)
  carry <- setdiff(active, refreshed)
  carry <- carry[!is.na(pkg_floor[carry])]
  nf[carry] <- pmin(nf[carry], pkg_floor[carry], na.rm = TRUE)
  nf <- nf[!is.na(nf)]
  all <- if (any(pkg_edge[long] == ct_prev)) ct_prev + 1 else as.Date(NA)
  all <- pmin(all, as.Date(stopped_from), na.rm = TRUE)
  own <- nf[is.na(all) | nf < all]
  list(all = if (!is.na(all)) format(all),
       packages = as.list(stats::setNames(format(own), names(own))))
}

run_update <- function(io, out_dir, reclassify_only = FALSE, deadline_min = DEADLINE_MIN,
                       update_batch = UPDATE_BATCH,
                       live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  # One clock for the whole run: the deadline is measured from entry, and the
  # last settled day T is SETTLED_LAG_DAYS before the UTC day the run started.
  t0 <- io$now()
  deadline <- t0 + deadline_min * 60
  settled <- as.Date(format(t0, "%Y-%m-%d", tz = "UTC")) - SETTLED_LAG_DAYS
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  recent_path   <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path  <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))
  manifest_path <- file.path(out_dir, "manifest.json")

  if (!io$release_exists()) {
    if (isTRUE(reclassify_only))
      stop("reclassify-only needs an existing roster; no release is published")
    stop("no published release to update: the history is built by backfill.yml; ",
         "run backfill.yml first")
  }
  # PROTECT-HISTORY: pull the manifest, the recent shard (the roster), every
  # year shard and the summary DB, then check them against the manifest.
  # gh cannot tell a transient failure from an asset the release lacks (a
  # publish that stopped partway, or an upload that deleted the asset and then
  # failed), so a failed download names both remedies. verify_release below
  # then reports only assets that were downloaded but do not match.
  repair <- paste0("re-run if gh failed transiently; if a publish that stopped partway left the ",
                   "release without it, complete that publish from its workflow artifact ",
                   "(see the README)")
  mc <- io$release_download("manifest.json", out_dir)
  rc <- io$release_download(basename(recent_path), out_dir)
  if (!identical(as.integer(mc), 0L) || !file.exists(manifest_path) ||
      !identical(as.integer(rc), 0L) || !file.exists(recent_path)) {
    stop("release 'current' exists but its manifest/recent shard could not be ",
         "downloaded; aborting to protect accumulated history: ", repair)
  }
  prev <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  named <- names(prev$shards %||% list())
  yc <- io$release_download(sprintf("%s-20*.db", SHARD_PREFIX), out_dir)
  sc <- io$release_download(basename(summary_path), out_dir)
  missed <- c(if (any(grepl(sprintf("^%s-20[0-9]{2}\\.db$", SHARD_PREFIX), named)) &&
                  !identical(as.integer(yc), 0L)) "the year shards",
              if (basename(summary_path) %in% named && !identical(as.integer(sc), 0L))
                basename(summary_path))
  if (length(missed))
    stop("could not download ", paste(missed, collapse = " and "), " of the published release; ",
         "nothing was changed: ", repair, ", or run backfill.yml")

  # Only a summed history can be updated: the window replacement below assumes
  # the stored days are sums over every release, and adding a never-fetched
  # release's history assumes it is not stored yet. A reclassify never fetches
  # and may republish an older release, whose marker it carries unchanged.
  summed <- identical(prev$history_method, HISTORY_METHOD) && length(prev$counted_through) == 1L
  if (!isTRUE(reclassify_only) && !summed)
    stop("the published release is not a summed history (its manifest has no history_method ",
         "\"", HISTORY_METHOD, "\" with counted_through); run backfill.yml first")
  verify_release(out_dir, prev, require_fingerprints = summed)

  roster <- load_releases(recent_path)
  if (nrow(roster) == 0L) {
    if (isTRUE(reclassify_only))
      stop("reclassify-only needs an existing roster; the published recent shard has none")
    stop("cold start: the published release has no roster; run backfill.yml first")
  }
  if (isTRUE(reclassify_only))
    return(run_reclassify(io, out_dir, prev, roster, summed, t0, live_floor, bioc_floor))

  ct_prev <- as.Date(as.character(prev$counted_through), format = "%Y-%m-%d")
  if (is.na(ct_prev))
    stop("the published counted_through is not a date; run backfill.yml first")
  prev_edges <- prev$package_edges %||% list()
  edge_dates <- as.Date(unlist(prev_edges, use.names = FALSE))
  # Refetch floors: days an earlier run counted beyond this run's usual window
  # start, which this run fetches a second time.
  floor_all <- as.Date(as.character(prev$refetch_from %||% NA), format = "%Y-%m-%d")
  prev_floors <- prev$package_refetch_from %||% list()
  if ((!is.null(prev$refetch_from) && is.na(floor_all)) ||
      anyNA(as.Date(as.character(unlist(prev_floors)), format = "%Y-%m-%d")))
    stop("the published refetch_from or package_refetch_from is not a date; run backfill.yml first")
  # A quiet stretch counted as downloads that stopped, while it is the latest
  # detected gap and ends at counted_through, is fetched again from its first
  # day every run (its start is the refetch floor), so days Launchpad fills in
  # later are counted and the gap is trimmed to what is still empty.
  prev_gaps <- prev$detected_gaps %||% list()
  last_gap <- if (length(prev_gaps)) prev_gaps[[length(prev_gaps)]]
  pending <- identical(last_gap$kind, "stopped") &&
             identical(as.character(last_gap$to), format(ct_prev))
  stopped_from <- if (pending) as.Date(as.character(last_gap$from), format = "%Y-%m-%d")
                  else as.Date(NA)
  if (pending && is.na(stopped_from))
    stop("the published stopped stretch in detected_gaps has no first day; run backfill.yml first")
  floor_all <- pmin(floor_all, stopped_from, na.rm = TRUE)
  no_op <- function(why) {
    message("nothing to publish: ", why)
    list(publish = FALSE, changed_shards = character(0), manifest = prev)
  }
  # With nothing settled past counted_through, only a package held at an
  # earlier edge can move.
  if (settled <= ct_prev && !any(edge_dates < settled))
    return(no_op(sprintf("nothing is settled past counted_through %s (last settled day %s)",
                         ct_prev, settled)))

  plan <- update_plan(roster, ct_prev, prev_edges, settled, floor_all, prev_floors)
  message(sprintf(paste0("counted through %s, settled through %s: %d window and %d full-history ",
                         "releases (%d of them answered empty once before); excluded %d never ",
                         "downloaded and %d dormant"),
                  ct_prev, settled, length(plan$w), length(plan$f), length(plan$empty_once),
                  length(plan$never), length(plan$dormant)))
  if (length(plan$w) == 0L) {
    if (length(plan$waiting) == 0L) {
      # Nothing is active, so no window fetch looks past counted_through and
      # only a dormant release could have new downloads there. Probe them
      # before counting on, also when releases await a full-history fetch
      # (after a backfill, every release that answered empty does).
      probe_dormant(io, roster, plan, min(ct_prev, floor_all, na.rm = TRUE), t0, deadline,
                    update_batch)
      if (length(plan$f) == 0L) return(dormant_heartbeat(out_dir, prev, t0))
    } else if (length(plan$f) == 0L) {
      return(no_op("no held package has a window release to catch up"))
    }
  }

  # Per-package edge E_p, refetch floor, window start S_p and the stored
  # history.
  pkgs <- unique(c(roster$package, names(prev_edges)))
  pkg_edge  <- stats::setNames(package_edge(pkgs, ct_prev, prev_edges), pkgs)
  pkg_floor <- stats::setNames(package_floor(pkgs, floor_all, prev_floors), pkgs)
  edge_c    <- stats::setNames(format(pkg_edge), pkgs)
  start_c   <- stats::setNames(format(pmin(pkg_edge - REVISION_WINDOW_DAYS, pkg_floor,
                                           na.rm = TRUE)), pkgs)
  hist <- load_history(out_dir, recent_path)

  # WINDOW: every active release over [S_p, T], under the hard deadline. A
  # deadline that stops the pool batches stops the run; the serial retry that
  # follows is bounded by WINDOW_RETRY_BUDGET_MIN and the deadline, and what it
  # does not recover counts as failed below, so a few releases that keep
  # failing are held instead of using up the run.
  w_urls <- counts_urls(roster[plan$w, , drop = FALSE], start = plan$start[plan$w], end = settled)
  w <- fetch_phase(io, w_urls, "window", t0, deadline, update_batch,
                   retry_budget_min = WINDOW_RETRY_BUDGET_MIN)
  if (w$cut)
    stop(sprintf("deadline: the window fetch did not finish within %s minutes; nothing written",
                 format(deadline_min)))
  failed <- plan$w[!w$ok]
  if (length(failed) > W_FAIL_MAX_FRAC * length(plan$w))
    stop(sprintf(paste0("outage: %d of %d window releases failed (more than %s%%); nothing ",
                        "written. First failed pub_ids: %s"),
                 length(failed), length(plan$w), format(100 * W_FAIL_MAX_FRAC),
                 paste(utils::head(roster$pub_id[failed], 5L), collapse = ", ")))
  # Stored days are package sums with no per-release provenance, so a package
  # is replaced only when every one of its window releases came back. One that
  # did not is HELD: its stored days stay, and it keeps its edge.
  held <- unique(roster$package[failed])
  w_daily <- aggregate_counts(bind_counts(w$rows[w$ok]), roster)

  # SOURCE-REGRESSION GUARD over each package's overlap [S_p, E_p]. The stored
  # days there come from window releases only (a never-fetched release has no
  # stored days, a dormant one none this recent), and Launchpad counts only
  # grow, so a re-fetched overlap well below the stored one means the source
  # lost data. A few such packages are held; more stop the run, as does a drop
  # across all the packages about to be replaced.
  w_pkgs <- setdiff(unique(roster$package[plan$w]), held)
  stored  <- overlap_sums(hist, w_pkgs, unname(start_c[w_pkgs]), unname(edge_c[w_pkgs]))
  fetched <- overlap_sums(w_daily, w_pkgs, unname(start_c[w_pkgs]), unname(edge_c[w_pkgs]))
  low <- fetched < (1 - REVISION_DROP_TOL) * stored
  regressed <- w_pkgs[stored >= REGRESSION_MIN_STORED & low]
  if (length(regressed) > REGRESSION_MAX_PACKAGES)
    stop(sprintf(paste0("source regression: %d packages re-fetched below their stored ",
                        "overlap (e.g. %s); nothing written"),
                 length(regressed), paste(utils::head(regressed, 5L), collapse = ", ")))
  if (length(regressed))
    message(sprintf("regressed, held at their edge: %s", paste(regressed, collapse = ", ")))
  rest <- !w_pkgs %in% regressed
  if (sum(fetched[rest]) < (1 - REVISION_DROP_TOL) * sum(stored[rest]))
    stop(sprintf(paste0("source regression: the window overlap re-fetched %s downloads against %s ",
                        "stored; nothing written"),
                 format(sum(fetched[rest])), format(sum(stored[rest]))))
  held <- c(held, regressed)
  if (length(held))
    message(sprintf("held back: %d package(s) keep their stored days: %s", length(held),
                    paste(utils::head(held, 20L), collapse = ", ")))

  # GAPS: ecosystem-wide download totals over (E, T]. A trailing run of empty
  # days has not been reported yet, so counted_through stops before it and the
  # next run fetches it again; an earlier run is recorded as a detected gap. A
  # quiet stretch longer than STALL_MAX_DAYS since the last download anywhere
  # (the roster's last days and the fetched window) is downloads that stopped:
  # it is counted and recorded, so the archive's end does not freeze the edge.
  # A stretch counted as stopped before is looked at again from its first day:
  # its entry is replaced by what the re-fetched days show, so days Launchpad
  # filled in trim it, and it goes on as stopped only while it still is. A
  # catch-up run, which settles nothing past counted_through, judges no gaps
  # and leaves the stretch as it is.
  widen <- !is.na(stopped_from) && settled > ct_prev
  gfrom <- if (widen) stopped_from - 1 else ct_prev
  base_gaps <- prev_gaps
  gaps <- if (any(w$ok)) {
    if (widen) base_gaps <- prev_gaps[-length(prev_gaps)]
    tot <- if (nrow(w_daily)) rowsum(as.numeric(w_daily$count), w_daily$date) else NULL
    seen <- c(roster$last_day, w_daily$date)
    seen <- seen[!is.na(seen) & seen <= format(gfrom)]
    window_gaps(if (is.null(tot)) numeric(0) else stats::setNames(tot[, 1], rownames(tot)),
                gfrom, settled, last_seen = if (length(seen)) max(seen) else NA)
  } else {
    # No window release, so nothing to judge the days by but the dormant
    # probe, which found nothing since the stopped stretch began: it goes on.
    on <- if (widen) list(list(from = format(ct_prev + 1), to = format(settled), kind = "stopped"))
    list(t_eff = settled, detected = on %||% list(), trailing = NULL, stopped = NULL)
  }
  if (widen && gaps$t_eff < ct_prev) {
    # Launchpad filled in days of the stretch recently enough that the quiet
    # since is a stall, not a stop, and it began before counted_through, which
    # cannot go back. The stretch stays recorded and fetched again as it was
    # until that quiet is long enough to count as stopped; the filled days are
    # counted then.
    base_gaps <- prev_gaps
    gaps$detected <- list()
    gaps$t_eff <- ct_prev
  }
  if (!is.null(gaps$trailing))
    cat(sprintf(paste0("::warning::c2d4u: Launchpad reported no downloads from %s to %s; ",
                       "counted_through stops at %s\n"),
                gaps$trailing$from, gaps$trailing$to, format(max(gaps$t_eff, ct_prev))))
  if (!is.null(gaps$stopped))
    cat(sprintf(paste0("::warning::c2d4u: Launchpad reported no downloads from %s to %s, ",
                       "more than %d days outside the known gaps; counting that as downloads ",
                       "having stopped, through %s\n"),
                gaps$stopped$from, gaps$stopped$to, STALL_MAX_DAYS, format(gaps$t_eff)))
  t_eff <- gaps$t_eff
  ct_new <- max(ct_prev, t_eff)
  movable <- names(prev_edges)[!names(prev_edges) %in% held & pkg_edge[names(prev_edges)] < t_eff]
  if (t_eff <= ct_prev && length(movable) == 0L)
    return(no_op(sprintf("nothing new counted past %s", ct_prev)))

  # FULL HISTORY: never-fetched releases, then those that answered empty once,
  # under a soft deadline. Whatever is not reached keeps its state for the next
  # run (unfetched, or empty once).
  f_urls <- counts_urls(roster[plan$f, , drop = FALSE])
  f <- fetch_phase(io, f_urls, "full history", t0, deadline, update_batch)
  if (f$cut)
    message(sprintf(paste0("deadline: stopped the full-history fetch; %d full-history ",
                           "release(s) stay unfetched"), sum(!f$ok)))
  f_daily <- aggregate_counts(bind_counts(f$rows[f$ok]), roster)

  # MERGE. A package is refreshed when it was observed (a release fetched), is
  # not held, and its edge is before T_eff: its stored days from S_p are
  # replaced by the fetched days of its window and full-history releases over
  # [S_p, T_eff], and a full-history release's days before S_p are added onto
  # what is stored. Launchpad counts only grow, so on a day of the overlap
  # [S_p, E_p] the window releases count the larger of the stored and the
  # re-fetched count: a short answer (an empty page, a release that lost a
  # day) must not lower a stored day, which no later run re-fetches once the
  # edge has moved past it. Every other package (held, or with nothing settled
  # past its edge) keeps its stored days and gets a full-history release's
  # days through its edge E_p added onto them: the stored days never held that
  # release, and its later days wait for the run that refreshes the package,
  # which re-fetches it as a window release from S_p. Nothing after
  # counted_through is kept (a full-history fetch runs to today).
  observed <- unique(c(roster$package[plan$w[w$ok]], roster$package[plan$f[f$ok]]))
  refreshed <- setdiff(observed, held)
  refreshed <- refreshed[pkg_edge[refreshed] < t_eff]
  te_c <- format(t_eff)
  mh <- match(hist$package, refreshed)
  drop <- !is.na(mh)
  drop[drop] <- hist$date[drop] >= start_c[refreshed][mh[drop]]
  in_window <- function(d) {
    m <- match(d$package, refreshed)
    k <- !is.na(m)
    k[k] <- d$date[k] >= start_c[refreshed][m[k]] & d$date[k] <= te_c
    d[k, , drop = FALSE]
  }
  overlap <- hist[drop, , drop = FALSE]
  overlap <- overlap[overlap$date <= edge_c[overlap$package], , drop = FALSE]
  win <- sum_daily(rbind(max_daily(overlap, in_window(w_daily)), in_window(f_daily)))
  f_ref <- f_daily$package %in% refreshed
  add <- f_daily[ifelse(f_ref, f_daily$date < start_c[f_daily$package],
                        f_daily$date <= edge_c[f_daily$package]), , drop = FALSE]
  touched <- sort(unique(substr(c(hist$date[drop], win$date, add$date), 1, 4)))
  daily_all <- add_onto(rbind(hist[!drop, , drop = FALSE], win), add)
  daily_all <- daily_all[daily_all$date <= format(ct_new), , drop = FALSE]

  # ROSTER: a fetched release with rows is settled (done = 1) and last_day
  # moves to its newest returned day (over all returned rows, so a release
  # first downloaded after T is active next run rather than "never
  # downloaded"). A release that has never returned a row is empty once
  # (done = 2) after its first empty answer and settled as never downloaded
  # (done = 1, last_day NA) after its second.
  mark <- function(roster, idx, res) {
    got <- idx[res$ok]
    newest <- vapply(res$rows[res$ok], function(r)
      if (nrow(r)) max(r$day, na.rm = TRUE) else NA_character_, character(1))
    old <- roster$last_day[got]
    none <- is.na(newest) & is.na(old)
    roster$done[got] <- ifelse(none & !roster$done[got] %in% 2L, 2L, 1L)
    roster$last_day[got] <- ifelse(is.na(newest) | (!is.na(old) & old >= newest), old, newest)
    roster
  }
  roster <- mark(roster, plan$w, w)
  roster <- mark(roster, plan$f, f)

  # Identity from the ledger (size-gated). On any failure DEGRADE honestly:
  # keep the persisted identity, never abort and never drop a row.
  ledger <- tryCatch(load_gated_maps(io, live_floor, bioc_floor), error = function(e) {
    message("identity ledger unavailable (", conditionMessage(e),
            "); keeping the roster's persisted canonical_name and identity_state")
    NULL
  })
  roster <- enrich_roster(roster, ledger)

  # package_edges: held packages keep their edge; a package refreshed only up
  # to a T_eff before counted_through gets T_eff. Listed only while the edge
  # is before counted_through. The summary anchors each listed package on its
  # edge, where its data stops.
  keep_pk <- setdiff(union(names(prev_edges), held), refreshed)
  ed <- stats::setNames(format(pkg_edge[keep_pk]), keep_pk)
  if (t_eff < ct_new && length(refreshed))
    ed <- c(ed, stats::setNames(rep(te_c, length(refreshed)), refreshed))
  ed <- as.list(ed[ed < format(ct_new)])

  all_gaps <- add_gaps(base_gaps, gaps$detected)
  last_gap <- if (length(all_gaps)) all_gaps[[length(all_gaps)]] else list()
  still_stopped <- if (identical(last_gap$kind, "stopped") &&
                       identical(last_gap$to, format(ct_new))) as.Date(last_gap$from)
                   else as.Date(NA)
  # Days of the re-fetched stretch that Launchpad filled in came back with
  # downloads for the first time in this run. Like any newly counted day they
  # are fetched in two runs, so the stretch's old first day stays the floor for
  # one more run, whether the rest is still stopped or downloads came back.
  # A package held in this run stores them only when a later run refreshes
  # it, which keeps its floor one more run (next_floors).
  filled <- widen && any(w_daily$date >= format(stopped_from) & w_daily$date <= format(ct_prev))
  floors <- next_floors(pkg_edge, pkg_floor, refreshed,
                        unique(roster$package[c(plan$w, plan$waiting)]), ct_prev, t_eff,
                        stopped_from = if (filled) pmin(still_stopped, stopped_from, na.rm = TRUE)
                                       else still_stopped,
                        held_before = names(prev_edges))
  if (!is.null(floors$all) || length(floors$packages))
    message(sprintf(paste0("the next run re-fetches every window release from %s and %d ",
                           "package(s) from an earlier day of their own"),
                    floors$all %||% "the usual start", length(floors$packages)))

  prior_summary <- load_summary(recent_path)
  ex <- export_release(out_dir, daily_all, roster, ct_new, prior_summary, touched, edges = ed)
  unfetched <- count_unfetched(roster$done)
  empty_once <- count_empty_once(roster$done)
  changed <- changed_with_previous(prev, out_dir, ex$files, t0, CHANGED_SHARDS_CARRY_DAYS)

  base <- list(
    tag = sprintf("v%s", format(t0, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(t0), last_checked = iso(t0), last_changed = iso(t0),
    source_kind = "launchpad", archives = as.list(enabled_archive_keys()),
    changed_shards = as.list(changed), shards = prev$shards %||% list(),
    summary = list(packages = nrow(ex$summary), latest_date = format(ct_new),
                   releases = nrow(roster), active_releases = length(plan$w) + length(plan$f),
                   window_releases = length(plan$w), full_history_releases = length(plan$f),
                   empty_once_rechecked = length(plan$empty_once),
                   excluded_never_downloaded = length(plan$never),
                   excluded_dormant = length(plan$dormant),
                   unfetched_releases = unfetched, empty_once_releases = empty_once,
                   held_packages = length(held)))
  out <- contract_manifest(base, out_dir, ex$files, ct_new, package_edges = ed,
                           unfetched_releases = unfetched, empty_once_releases = empty_once,
                           detected_gaps = all_gaps, refetch_from = floors$all,
                           package_refetch_from = floors$packages)
  write_manifest(manifest_path, out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  write_upload_list(out_dir, ex$files)
  message(sprintf(paste0("counted through %s; %d package(s) refreshed, %d held, %d release(s) ",
                         "unfetched, %d answered empty once"),
                  format(ct_new), length(refreshed), length(held), unfetched, empty_once))
  list(publish = TRUE, changed_shards = changed, manifest = out)
}

# No release is active any more (all dormant, never downloaded or not yet
# settled). Probe the DORMANT_PROBE_N dormant releases downloaded last for
# anything since `from` (counted_through, or the refetch floor when that is
# earlier, such as the first day of a stretch counted as stopped): any means
# dormant releases came back, which the monthly update does not fetch, so stop
# and ask for a backfill.
probe_dormant <- function(io, roster, plan, from, t0, deadline, batch) {
  pool <- plan$dormant
  pick <- utils::head(pool[order(roster$last_day[pool], decreasing = TRUE)], DORMANT_PROBE_N)
  res <- fetch_phase(io, counts_urls(roster[pick, , drop = FALSE],
                                     start = rep(from, length(pick))),
                     "dormant probe", t0, deadline, batch)
  if (res$cut) stop("deadline: the dormant probe did not finish; nothing written")
  if (!all(res$ok))
    stop(sprintf("dormant probe: %d of %d releases could not be fetched; nothing written",
                 sum(!res$ok), length(pick)))
  found <- sum(vapply(res$rows, nrow, integer(1)))
  if (found > 0L)
    stop(sprintf(paste0("dormant releases have new downloads since %s (%d day rows); the monthly ",
                        "update does not fetch dormant releases, so run backfill.yml"),
                 format(from), found))
  message(sprintf("dormant probe: no new downloads in %d dormant release(s)", length(pick)))
  invisible(TRUE)
}

# A quiet probe with nothing else to fetch: republish the manifest, and only
# the manifest, as a heartbeat that keeps counted_through (and the previous
# changed_shards, which a loader may not have read yet).
dormant_heartbeat <- function(out_dir, prev, t0) {
  out <- prev
  out$last_checked <- iso(t0)
  out$source_kind <- "dormant"
  out$changed_shards <- prev$changed_shards %||% list()
  write_manifest(file.path(out_dir, "manifest.json"), out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  write_upload_list(out_dir, character(0))
  message("heartbeat: every release is dormant and the probe found no new downloads")
  list(publish = TRUE, changed_shards = as.character(unlist(out$changed_shards)), manifest = out)
}

# Rebuild identity and the summary over the downloaded history, with zero
# Launchpad calls. The history is not rebuilt, so last_checked, counted_through
# (else, on a release from before the summed backfill, summary.latest_date),
# package_edges, the refetch floors and the history marker are carried over
# unchanged: a
# reclassify must never make stale data look fresh. changed_shards keeps the
# previous year entries, because a loader reads the year shards the CURRENT
# manifest names and may not have read them yet.
run_reclassify <- function(io, out_dir, prev, roster, summed, t0, live_floor, bioc_floor) {
  recent_path <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  anchor <- prev$counted_through %||% prev$summary$latest_date
  if (is.null(anchor))
    stop("reclassify-only: the published manifest has neither counted_through nor summary.latest_date")
  # reclassify-only must never degrade: the point of the run is to (re)apply
  # the ledger, so a missing or undersized ledger is fatal.
  ledger <- tryCatch(load_gated_maps(io, live_floor, bioc_floor), error = function(e)
    stop("reclassify-only requires the identity ledger; aborting rather ",
         "than republish degraded identity (", conditionMessage(e), ")"))
  hist <- load_history(out_dir, recent_path)
  if (nrow(hist) == 0L) stop("reclassify-only: no existing daily rows to rebuild the summary")
  roster <- enrich_roster(roster, ledger)
  ex <- export_release(out_dir, hist, roster, anchor, load_summary(recent_path), character(0),
                       edges = prev$package_edges %||% list())

  changed <- changed_with_previous(prev, out_dir, ex$files, t0)
  base <- prev
  base$tag <- sprintf("v%s", format(t0, "%Y%m%d-%H%M%S", tz = "UTC"))
  base$generated_at <- iso(t0)
  base$last_changed <- iso(t0)
  base$source_kind <- "reclassify"
  base$archives <- as.list(enabled_archive_keys())
  base$changed_shards <- as.list(changed)
  base$summary <- list(packages = nrow(ex$summary), latest_date = anchor,
                       releases = nrow(roster), active_releases = 0L)
  out <- contract_manifest(base, out_dir, changed, anchor,
                           package_edges = prev$package_edges %||% list(),
                           unfetched_releases = count_unfetched(roster$done),
                           empty_once_releases = count_empty_once(roster$done),
                           detected_gaps = prev$detected_gaps %||% list(),
                           refetch_from = prev$refetch_from,
                           package_refetch_from = prev$package_refetch_from %||% list(),
                           history_method = if (summed) HISTORY_METHOD else NULL)
  write_manifest(file.path(out_dir, "manifest.json"), out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  write_upload_list(out_dir, ex$files)
  list(publish = TRUE, changed_shards = changed, manifest = out)
}

# Whether the `current` release exists, from `gh release view`. gh exits 1
# both when the release is missing and when it cannot answer (bad token,
# network, API error); only the first prints "release not found". Reading any
# failure as "no release" would let the backfill drop the published roster
# from its enumerate and skip its check against the published history, so any
# other failure is retried and then stops the run. `view` makes the gh call and
# returns list(status, output).
gh_release_exists <- function(view, tries = 3L, wait = 3) {
  for (i in seq_len(tries)) {
    r <- view()
    if (identical(r$status, 0L)) return(TRUE)
    if (any(grepl("release not found", r$output, fixed = TRUE))) return(FALSE)
    if (i < tries) Sys.sleep(wait * i)
  }
  stop("could not tell whether the current release exists: gh exited ", r$status, ": ",
       paste(utils::tail(r$output, 2L), collapse = " "), call. = FALSE)
}

default_io <- function() {
  gh_out <- function(args) {
    st <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
    list(status = as.integer(attr(st, "status") %||% 0L), output = as.character(st))
  }
  gh_rc <- function(args) gh_out(args)$status
  list(
    release_exists = function() gh_release_exists(function()
      gh_out(c("release", "view", "current", "--repo", PUBLISH_REPO))),
    release_download = function(pattern, dir) {
      for (i in seq_len(3L)) {
        code <- gh_rc(c("release", "download", "current", "--repo", PUBLISH_REPO,
                        "--pattern", pattern, "--dir", dir, "--clobber"))
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      1L
    },
    # Serial fetch: the body on HTTP 200, NULL otherwise. HTTP 404/410 fail fast
    # (the page is gone, so a retry only burns time); anything else is retried.
    fetch = function(url) {
      tryCatch(with_retry({
        h <- curl::new_handle(useragent = USER_AGENT, timeout = 90L, connecttimeout = 20L)
        r <- curl::curl_fetch_memory(url, handle = h)
        if (r$status_code %in% c(404L, 410L))
          stop(errorCondition(paste0("HTTP ", r$status_code), class = "c2d4u_gone"))
        if (r$status_code != 200L) stop("HTTP ", r$status_code)
        rawToChar(r$content)
      }), error = function(e) NULL)
    },
    # Concurrent multi-url fetch. Pool size is capped by config POOL but
    # overridable per job via C2D4U_POOL so the workflow can hold
    # max-parallel * POOL under Launchpad's ~24-connection throttle. `deadline`
    # (POSIXct, on the Sys.time clock that `now` below also reads) is passed to
    # fetch_pool, which leaves urls it did not reach NULL.
    fetch_many = function(urls, deadline = Inf) {
      pool <- suppressWarnings(as.integer(Sys.getenv("C2D4U_POOL", as.character(POOL))))
      if (is.na(pool) || pool < 1L) pool <- POOL
      fetch_pool(urls, pool = pool, deadline = deadline)
    },
    cran_names = function() rownames(utils::available.packages(repos = CRAN_REPO)),
    archive_names = function() parse_archive_index(fetch_pool(CRAN_ARCHIVE_INDEX)[[1]]),
    bioc_names = function() {
      urls <- sprintf("%s/%s/VIEWS", BIOC_VIEWS_BASE, BIOC_VIEWS_CATEGORIES)
      unique(unlist(lapply(urls, function(u) {
        txt <- tryCatch(rawToChar(curl::curl_fetch_memory(u)$content), error = function(e) "")
        parse_views_packages(txt)
      }), use.names = FALSE))
    },
    # Downloads the shared identity assets (cran-archive's cran_names_all and
    # bioconductor-metadata's bioc_names_all) from each source repo's `current`
    # release into a temp dir, for robservatory::load_identity.
    identity_dbs = function() {
      tmp <- tempfile(); dir.create(tmp, showWarnings = FALSE)
      dl <- function(repo, db) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", repo,
            "--pattern", db, "--dir", tmp, "--clobber"), stdout = FALSE, stderr = FALSE))
        p <- file.path(tmp, db)
        if (!identical(as.integer(st), 0L) || !file.exists(p)) stop("identity asset unreachable: ", repo, "/", db)
        p
      }
      list(cran = dl(CRAN_ARCHIVE_REPO, CRAN_ARCHIVE_DB),
           bioc = dl(BIOC_META_REPO, BIOC_META_DB))
    },
    now = function() Sys.time())
}

# The CLI. A run with nothing to publish leaves out_dir/skip-publish for the
# workflow's publish step. `env` reads an environment variable (Sys.getenv
# shape) and `io` defaults to the real one; both are injectable for tests.
update_cli <- function(out_dir = "out", env = Sys.getenv, io = NULL) {
  truthy <- function(x) tolower(x) %in% c("true", "1", "yes")
  if (truthy(env(FORCE_REBUILD_ENV, "")))
    stop(FORCE_REBUILD_ENV, " is set, but the monthly update never rebuilds the history: ",
         "a full re-fetch and rebuild is done by backfill.yml")
  deadline_min <- suppressWarnings(as.numeric(env(DEADLINE_MIN_ENV, as.character(DEADLINE_MIN))))
  if (length(deadline_min) != 1L || is.na(deadline_min) || deadline_min <= 0)
    stop(DEADLINE_MIN_ENV, " must be a positive number of minutes")
  skip <- file.path(out_dir, "skip-publish")
  if (file.exists(skip)) unlink(skip)
  res <- run_update(io %||% default_io(), out_dir,
                    reclassify_only = truthy(env(RECLASSIFY_ONLY_ENV, "")),
                    deadline_min = deadline_min)
  if (!isTRUE(res$publish)) {
    writeLines("nothing to publish this run", skip)
    cat("nothing to publish\n")
  } else {
    cat("changed shards:", paste(res$changed_shards, collapse = ", "), "\n")
  }
  invisible(res)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  update_cli(if (length(args) >= 1L) args[[1]] else "out")
}
