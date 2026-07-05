#!/usr/bin/env Rscript
# scripts/local-bootstrap.R: resumable, concurrent LOCAL bootstrap for the
# c2d4u-downloads pipeline.
#
# The whole-archive getPublishedBinaries sweep hits sustained HTTP 503 walls
# (the server-side scan is too expensive past ~12,900 entries). This bootstrap
# instead enumerates the roster with cheap, reliable per-package-name filtered
# queries (binary_name=<name>&exact_match=true), one query per candidate binary
# name. It fetches all download counts, builds the published shards + summary +
# manifest into out/, and (only in `run` mode) publishes to the GitHub release.
#
# Every stage is resumable via checkpoint files under bootstrap-state/:
#   queried-names.rds  names that returned HTTP 200 (skip on resume)
#   entries.rds        accumulated getPublishedBinaries entries
#   roster.rds         final deduped roster (enumerate-complete marker)
#   counts-done.rds    pub_ids whose counts were fully fetched
#   daily-<key>.rds    per-first-letter aggregated daily frames
#
# Usage: Rscript scripts/local-bootstrap.R <validate|run> [limit]
#   validate  restrict to a tiny 5-name slice; build to out/, do NOT publish
#   run       full candidate universe; build to out/ and publish
#   limit     optional cap on the number of candidate names (testing)

options(timeout = 600)

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(jsonlite); library(curl)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
source(file.path(.script_dir, "config.R"))
source(file.path(.script_dir, "helpers.R"))

# ---------------------------------------------------------------------------
# Constants for this bootstrap.
STATE_DIR   <- "bootstrap-state"
OUT_DIR     <- "out"
POOL        <- 8L      # concurrent connections
ENUM_BATCH  <- 300L    # names per enumerate wave
ENUM_CKPT   <- 2000L   # checkpoint the queried set + entries every N names
CNT_BATCH   <- 400L    # pub_ids per counts wave
FETCH_PASSES <- 4L     # total fetch attempts per url (1 + 3 retries) for 503s

ARCHIVE     <- ARCHIVES[[1]]           # the enabled c2d4u4.0+ archive
VALIDATE_NAMES <- c("r-cran-ggplot2", "r-cran-mass", "r-bioc-biobase",
                    "r-cran-zoo", "r-cran-jsonlite")

T0 <- Sys.time()
lg <- function(...) cat(sprintf("[%7.1fs] %s\n",
  as.numeric(difftime(Sys.time(), T0, units = "secs")), sprintf(...)))

# ---------------------------------------------------------------------------
# CONCURRENCY POOL. Fetch every url with a bounded curl::multi pool, then make
# several retry passes over the failed/NULL indices with growing sleeps so a
# 503 wave is ridden out rather than dropping data. Returns a list aligned to
# `urls`: the response body string on HTTP 200, NULL otherwise. Modelled on the
# autoobs mc_multi pattern with more retry passes for Launchpad's 503s.
fetch_pool <- function(urls, pool = POOL, passes = FETCH_PASSES, block = 1500L) {
  out <- vector("list", length(urls))
  n <- length(urls)
  if (n == 0L) return(out)
  run <- function(idxs) {
    for (s in seq(1L, length(idxs), by = block)) {
      e   <- min(s + block - 1L, length(idxs))
      sel <- idxs[s:e]
      p   <- curl::new_pool(total_con = pool, host_con = pool)
      for (j in sel) {
        local({
          jj <- j
          h <- curl::new_handle(useragent = USER_AGENT, timeout = 90L, connecttimeout = 20L)
          curl::handle_setopt(h, url = urls[jj])
          curl::multi_add(h,
            done = function(res) if (isTRUE(res$status_code == 200L)) out[[jj]] <<- rawToChar(res$content),
            fail = function(err) invisible(NULL),
            pool = p)
        })
      }
      curl::multi_run(pool = p)
    }
  }
  run(seq_len(n))
  for (k in seq_len(passes - 1L)) {
    failed <- which(vapply(out, is.null, logical(1)))
    if (length(failed) == 0L) break
    Sys.sleep(3 * k)   # growing backoff between passes
    run(failed)
  }
  out
}

# Fetch a set of first-page urls concurrently, parse each with parse_fn, and
# follow next_collection_link concurrently until every item is exhausted. Only a
# handful of names/releases exceed one page, so later waves shrink quickly.
# Returns list(data = per-item field data.frame or partial/NULL, ok = logical:
# TRUE only where the item fully completed with no failed page).
fetch_paginated <- function(first_urls, parse_fn, field, pool = POOL) {
  n <- length(first_urls)
  acc <- vector("list", n)         # accumulated field rows per item
  ok  <- logical(n)                # settled-complete flag per item
  cur <- first_urls                # current url to fetch per item
  active <- seq_len(n)
  guard <- 0L
  while (length(active) > 0L) {
    guard <- guard + 1L
    if (guard > 100000L) stop("fetch_paginated: runaway paging")
    bodies <- fetch_pool(cur[active], pool = pool)
    nxt <- integer(0)
    for (m in seq_along(active)) {
      i <- active[m]; body <- bodies[[m]]
      if (is.null(body)) next          # failed page -> item stays ok=FALSE
      pr <- tryCatch(parse_fn(body), error = function(e) NULL)
      if (is.null(pr)) next
      acc[[i]] <- if (is.null(acc[[i]])) pr[[field]] else rbind(acc[[i]], pr[[field]])
      nl <- pr$next_link
      if (length(nl) == 1L && !is.na(nl)) { cur[i] <- nl; nxt <- c(nxt, i) }
      else ok[i] <- TRUE
    }
    active <- nxt
  }
  list(data = acc, ok = ok)
}

# ---------------------------------------------------------------------------
# Small state helpers.
ensure_dirs <- function() {
  dir.create(STATE_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(OUT_DIR,   showWarnings = FALSE, recursive = TRUE)
}
sp   <- function(f) file.path(STATE_DIR, f)
rd   <- function(f, default) { p <- sp(f); if (file.exists(p)) readRDS(p) else default }
wr   <- function(f, obj) saveRDS(obj, sp(f))

empty_daily <- function() data.frame(package = character(0), date = character(0),
                                     count = integer(0), stringsAsFactors = FALSE)
empty_entries <- function() data.frame(
  archive = character(0), pub_id = integer(0), binary_name = character(0),
  version = character(0), arch = character(0), status = character(0),
  date_published = character(0), stringsAsFactors = FALSE)

sum_daily <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(empty_daily())
  agg <- stats::aggregate(count ~ package + date, data = df, FUN = sum)
  agg$count <- as.integer(agg$count)
  agg
}

# ---------------------------------------------------------------------------
# Name maps (canonical-case resolution) and the candidate name universe.
cran_names_fn <- function() suppressWarnings(rownames(utils::available.packages(repos = CRAN_REPO)))

bioc_names_fn <- function() {
  urls <- sprintf("%s/%s/VIEWS", BIOC_VIEWS_BASE, BIOC_VIEWS_CATEGORIES)
  bodies <- fetch_pool(urls)
  unique(unlist(lapply(bodies, function(b) if (is.null(b)) character(0) else parse_views_packages(b)),
                use.names = FALSE))
}

# Every package name ever archived on CRAN (directory listing of src/contrib/Archive).
archive_names_fn <- function() {
  body <- fetch_pool("https://cran.r-project.org/src/contrib/Archive/")[[1]]
  if (is.null(body)) { lg("WARN: CRAN Archive listing unreachable; continuing without it"); return(character(0)) }
  m <- regmatches(body, gregexpr('href="([^"/]+)/"', body))[[1]]
  nm <- sub('href="([^"/]+)/"', "\\1", m)
  nm[!nm %in% c("..", ".") & nzchar(nm)]
}

build_maps <- function(cran, bioc) {
  list(cran = build_cran_map(cran),
       bioc = if (isTRUE(LOAD_BIOC_MAP)) build_bioc_map(bioc) else NULL)
}

candidate_names <- function(mode, cran, bioc) {
  if (mode == "validate") return(VALIDATE_NAMES)
  archive <- archive_names_fn()
  lg("name universe: CRAN=%d  CRAN-archive=%d  Bioc=%d", length(cran), length(archive), length(bioc))
  cand <- unique(c(paste0("r-cran-", tolower(unique(c(cran, archive)))),
                   paste0("r-bioc-", tolower(bioc))))
  lg("candidate binary names: %d (r-cran-*=%d, r-bioc-*=%d)",
     length(cand), sum(startsWith(cand, "r-cran-")), sum(startsWith(cand, "r-bioc-")))
  cand
}

# ---------------------------------------------------------------------------
# STAGE 1: ENUMERATE the roster via per-name filtered getPublishedBinaries.
enumerate <- function(candidates, maps) {
  if (file.exists(sp("roster.rds"))) {
    roster <- readRDS(sp("roster.rds"))
    lg("enumerate: resume-complete, roster.rds has %d releases", nrow(roster))
    return(roster)
  }
  queried  <- rd("queried-names.rds", character(0))
  entries  <- rd("entries.rds", empty_entries())
  remaining <- setdiff(candidates, queried)
  lg("enumerate: %d candidates, %d already queried, %d remaining",
     length(candidates), length(intersect(candidates, queried)), length(remaining))

  since_ckpt <- 0L
  batches <- split(remaining, ceiling(seq_along(remaining) / ENUM_BATCH))
  ref <- lp_archive_ref(ARCHIVE)
  for (bi in seq_along(batches)) {
    nm <- batches[[bi]]
    urls <- sprintf("%s?ws.op=getPublishedBinaries&binary_name=%s&exact_match=true&ordered=false&ws.size=%d",
                    ref, vapply(nm, curl::curl_escape, character(1)), PAGE_SIZE)
    res <- fetch_paginated(urls, parse_published_page, "entries")
    got <- res$ok                              # HTTP-200-and-complete names
    if (any(got)) {
      ent_list <- res$data[got]
      ent <- do.call(rbind, ent_list[!vapply(ent_list, is.null, logical(1))])
      if (!is.null(ent) && nrow(ent) > 0L) {
        ent <- cbind(archive = ARCHIVE$key, ent, stringsAsFactors = FALSE)
        entries <- rbind(entries, ent)
      }
      queried <- union(queried, nm[got])
    }
    since_ckpt <- since_ckpt + length(nm)
    hits <- if (nrow(entries) > 0L) length(unique(entries$binary_name)) else 0L
    lg("enumerate batch %d/%d: +%d queried (%d ok), entries=%d, distinct-names-present=%d",
       bi, length(batches), length(nm), sum(got), nrow(entries), hits)
    if (since_ckpt >= ENUM_CKPT || bi == length(batches)) {
      wr("queried-names.rds", queried)
      wr("entries.rds", entries)
      since_ckpt <- 0L
      lg("enumerate: checkpoint saved (queried=%d, entries=%d)", length(queried), nrow(entries))
    }
  }
  # Retry loop safety: any names that never returned 200 are still unqueried and
  # will be retried on the next invocation. If everything was attempted, finalise.
  still <- setdiff(candidates, queried)
  if (length(still) > 0L) {
    lg("enumerate: %d names still unqueried after this pass; re-run to retry them", length(still))
    return(NULL)  # not complete
  }
  roster <- build_roster(entries, maps$cran, maps$bioc)
  saveRDS(roster, sp("roster.rds"))
  lg("enumerate: COMPLETE. entries=%d -> roster releases=%d (distinct packages=%d)",
     nrow(entries), nrow(roster), length(unique(roster$package)))
  roster
}

# ---------------------------------------------------------------------------
# STAGE 2: FETCH download counts per roster release, aggregate into per-letter
# daily frames. Resumable via counts-done.rds + daily-<key>.rds.
fetch_counts <- function(roster) {
  done <- rd("counts-done.rds", integer(0))
  daily_acc <- new.env(parent = emptyenv())
  for (f in list.files(STATE_DIR, pattern = "^daily-.*\\.rds$")) {
    key <- sub("^daily-(.*)\\.rds$", "\\1", f)
    assign(key, readRDS(sp(f)), envir = daily_acc)
  }
  todo <- roster[!(roster$pub_id %in% done), , drop = FALSE]
  lg("fetch: %d releases total, %d already done, %d to fetch",
     nrow(roster), sum(roster$pub_id %in% done), nrow(todo))

  if (nrow(todo) > 0L) {
    batches <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / CNT_BATCH))
    for (bi in seq_along(batches)) {
      idx <- batches[[bi]]
      sub <- todo[idx, , drop = FALSE]
      urls <- vapply(sub$pub_id, function(pid) lp_counts_url(ARCHIVE, pid), character(1))
      res <- fetch_paginated(urls, parse_counts_page, "rows")
      got <- res$ok
      rows <- if (any(got)) do.call(rbind, res$data[got][!vapply(res$data[got], is.null, logical(1))]) else NULL
      daily_batch <- aggregate_counts(if (is.null(rows)) empty_daily_rows() else rows, roster)
      touched <- character(0)
      if (nrow(daily_batch) > 0L) {
        keys <- shard_key(daily_batch$package)
        for (k in unique(keys)) {
          part <- daily_batch[keys == k, c("package", "date", "count"), drop = FALSE]
          prev <- if (exists(k, envir = daily_acc, inherits = FALSE)) get(k, envir = daily_acc) else empty_daily()
          assign(k, sum_daily(rbind(prev, part)), envir = daily_acc)
          touched <- c(touched, k)
        }
      }
      done <- union(done, sub$pub_id[got])
      # Checkpoint every batch: persist the done set and any touched daily shard.
      wr("counts-done.rds", done)
      for (k in unique(touched)) saveRDS(get(k, envir = daily_acc), sp(sprintf("daily-%s.rds", k)))
      nrows <- sum(vapply(ls(daily_acc), function(k) nrow(get(k, envir = daily_acc)), integer(1)))
      lg("fetch batch %d/%d: %d pub_ids (%d ok), count-rows=%s, daily-rows=%d, done=%d/%d",
         bi, length(batches), nrow(sub), sum(got),
         if (is.null(rows)) "0" else as.character(nrow(rows)), nrows, length(done), nrow(roster))
    }
  }

  remaining <- roster$pub_id[!(roster$pub_id %in% done)]
  daily <- do.call(rbind, c(lapply(ls(daily_acc), function(k) get(k, envir = daily_acc)), list(empty_daily())))
  list(daily = daily, remaining = remaining, done = done)
}

empty_daily_rows <- function() data.frame(binary_name = character(0), version = character(0),
                                          day = character(0), count = integer(0),
                                          stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# STAGE 3: BUILD the shards + summary + manifest into out/.
build_outputs <- function(daily_all, roster) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, daily_table_ddl(DAILY_TABLE))
  if (nrow(daily_all) > 0L)
    DBI::dbWriteTable(con, DAILY_TABLE, daily_all[c("package", "date", "count")], append = TRUE)

  now <- Sys.time()
  anchor <- if (nrow(daily_all) > 0L) max(daily_all$date) else format(as.Date(now), "%Y-%m-%d")
  changed <- character(0); shard_updates <- list()

  years <- if (nrow(daily_all) > 0L) sort(unique(substr(daily_all$date, 1, 4))) else character(0)
  for (yr in years) {
    f <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    rows <- extract_year(con, as.integer(yr))
    export_shard(file.path(OUT_DIR, f), rows)
    changed <- c(changed, f); shard_updates[[f]] <- coverage(rows)
  }

  summary_df <- build_summary(con, roster, anchor, prior_summary = NULL)

  recent_path  <- file.path(OUT_DIR, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path <- file.path(OUT_DIR, sprintf("%s-summary.db", SHARD_PREFIX))
  r_rows <- extract_recent(con, anchor, RECENT_WINDOW_DAYS)
  export_shard(recent_path, r_rows)
  embed_aux(recent_path, summary_df, roster)
  export_summary_shard(summary_path, summary_df)
  changed <- c(changed, basename(recent_path), basename(summary_path))
  shard_updates[[basename(recent_path)]] <- coverage(r_rows)

  manifest <- list(
    tag = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at = iso(now), last_checked = iso(now), last_changed = iso(now),
    source_kind = "launchpad", archives = list(ARCHIVE$key),
    changed_shards = as.list(changed),
    shards = shard_updates,
    summary = list(packages = nrow(summary_df), latest_date = anchor, releases = nrow(roster)))
  write_manifest(file.path(OUT_DIR, "manifest.json"), manifest)
  write_release_notes(file.path(OUT_DIR, "release_notes.md"), manifest)
  list(summary = summary_df, anchor = anchor, changed = changed, daily_rows = nrow(daily_all))
}

# ---------------------------------------------------------------------------
# STAGE 4: PUBLISH to the GitHub release (only in `run` mode).
gh_run <- function(args) suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
gh_ok  <- function(args) identical(as.integer(attr(gh_run(args), "status") %||% 0L), 0L)

publish <- function() {
  notes <- file.path(OUT_DIR, "release_notes.md")
  if (!gh_ok(c("release", "view", "current", "--repo", PUBLISH_REPO))) {
    lg("publish: release 'current' absent; creating tag + release")
    suppressWarnings(system2("git", c("tag", "current"), stdout = TRUE, stderr = TRUE))
    suppressWarnings(system2("git", c("push", "origin", "current"), stdout = TRUE, stderr = TRUE))
    gh_run(c("release", "create", "current", "--repo", PUBLISH_REPO,
             "--notes-file", notes, "--latest"))
  }
  dbs <- list.files(OUT_DIR, pattern = "\\.db$", full.names = TRUE)
  for (f in dbs) {
    lg("publish: upload %s", basename(f))
    gh_run(c("release", "upload", "current", "--repo", PUBLISH_REPO, f, "--clobber"))
  }
  # Manifest last, so a partial upload never advertises shards that are not there.
  lg("publish: upload manifest.json (last)")
  gh_run(c("release", "upload", "current", "--repo", PUBLISH_REPO,
           file.path(OUT_DIR, "manifest.json"), "--clobber"))
  gh_run(c("release", "edit", "current", "--repo", PUBLISH_REPO, "--notes-file", notes))
  lg("publish: done")
}

# ---------------------------------------------------------------------------
# Driver.
main <- function(mode, limit = NA_integer_) {
  if (!mode %in% c("validate", "run")) stop("usage: local-bootstrap.R <validate|run> [limit]")
  ensure_dirs()
  do_publish  <- (mode == "run")
  force_build <- tolower(Sys.getenv("C2D4U_FORCE_BUILD", "")) %in% c("1", "true", "yes")

  lg("mode=%s  force_build=%s  publish=%s", mode, force_build, do_publish)

  # Name maps are needed for canonical-case resolution in both modes.
  lg("building CRAN/Bioc name maps ...")
  cran <- cran_names_fn()
  bioc <- if (isTRUE(LOAD_BIOC_MAP)) bioc_names_fn() else character(0)
  maps <- build_maps(cran, bioc)
  lg("maps: CRAN=%d names, Bioc=%d names", length(cran), length(bioc))

  candidates <- candidate_names(mode, cran, bioc)
  if (!is.na(limit) && limit > 0L && length(candidates) > limit) {
    candidates <- candidates[seq_len(limit)]
    lg("limit applied: %d candidate names", length(candidates))
  }

  roster <- enumerate(candidates, maps)
  if (is.null(roster)) {
    lg("STOP: enumerate incomplete; re-run to resume."); return(invisible())
  }
  if (nrow(roster) == 0L) { lg("roster is empty; nothing to fetch. STOP."); return(invisible()) }

  fc <- fetch_counts(roster)
  if (length(fc$remaining) > 0L && !force_build) {
    lg("STOP: %d releases not yet fetched; re-run to resume (or set C2D4U_FORCE_BUILD=1 to build anyway).",
       length(fc$remaining))
    return(invisible())
  }
  if (length(fc$remaining) > 0L)
    lg("WARN: building with %d unfetched releases (C2D4U_FORCE_BUILD).", length(fc$remaining))

  lg("building outputs into %s/ ...", OUT_DIR)
  res <- build_outputs(fc$daily, roster)
  lg("build COMPLETE: daily-rows=%d, packages=%d, releases=%d, anchor=%s",
     res$daily_rows, nrow(res$summary), nrow(roster), res$anchor)
  lg("shards written: %s", paste(res$changed, collapse = ", "))

  # Show a small slice of the summary for the operator.
  s <- res$summary
  if (nrow(s) > 0L) {
    show <- head(s[order(-s$total_30d), c("package", "origin", "canonical_name", "total_30d", "cnt_total")], 15L)
    cat("\n--- summary (top by total_30d) ---\n")
    print(show, row.names = FALSE)
    cat("\n")
  }

  if (do_publish) { lg("publishing ..."); publish() } else lg("validate mode: NOT publishing.")
  invisible()
}

if (sys.nframe() == 0L) {
  args  <- commandArgs(trailingOnly = TRUE)
  mode  <- if (length(args) >= 1L) args[[1]] else "validate"
  limit <- if (length(args) >= 2L) suppressWarnings(as.integer(args[[2]])) else NA_integer_
  main(mode, limit)
}
