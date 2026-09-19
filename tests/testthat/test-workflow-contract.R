# The workflows drive the two CLIs only through environment variables and the
# out/skip-publish marker. A renamed or mistyped variable is read as unset and
# falls back to a default without any error, so both sides are pinned here.

wf_lines <- function(name) readLines(file.path(.c2_root, ".github", "workflows", name), warn = FALSE)

# The lines of one job, from its header under `jobs:` to the next job's.
wf_job <- function(lines, job) {
  jobs_at <- grep("^jobs:\\s*$", lines)
  heads <- grep("^  [A-Za-z0-9_-]+:\\s*$", lines)
  heads <- heads[heads > jobs_at]
  i <- heads[sub("^  ([A-Za-z0-9_-]+):.*$", "\\1", lines[heads]) == job]
  stopifnot(length(i) == 1L)
  end <- c(heads[heads > i], length(lines) + 1L)[1] - 1L
  lines[i:end]
}

# name -> value of every C2D4U_ variable these lines set, quotes stripped.
wf_env <- function(lines) {
  m <- regmatches(lines, regexec("^\\s+(C2D4U_[A-Z_]+):\\s*(.*?)\\s*$", lines))
  m <- m[lengths(m) == 3L]
  stats::setNames(gsub('^"|"$', "", vapply(m, `[`, "", 3L)), vapply(m, `[`, "", 2L))
}

# Every C2D4U_ name the given scripts mention, in code or in a config constant.
script_env_names <- function(files) {
  txt <- unlist(lapply(file.path(.c2_root, "scripts", files), readLines, warn = FALSE))
  txt <- txt[!grepl("^\\s*#", txt)]
  unique(unlist(regmatches(txt, gregexpr("C2D4U_[A-Z_]+", txt))))
}

# The steps of a job, each as its lines, from one "- " step line to the next,
# without comment lines (a comment above a step would otherwise count as part
# of the step before it).
wf_steps <- function(job) {
  at <- grep("^      - ", job)
  ends <- c(at[-1] - 1L, length(job))
  lapply(seq_along(at), function(k) { s <- job[at[k]:ends[k]]; s[!grepl("^\\s*#", s)] })
}
wf_step_at <- function(steps, pattern)
  which(vapply(steps, function(s) any(grepl(pattern, s)), logical(1)))
# The number a `key: <n>` line in these lines sets.
wf_num <- function(lines, key)
  as.numeric(sub(sprintf("^\\s*%s:\\s*([0-9]+).*$", key), "\\1",
                 grep(sprintf("^\\s*%s:", key), lines, value = TRUE)))

update_reads   <- function() script_env_names(c("config.R", "helpers.R", "update.R"))
backfill_reads <- function() script_env_names(c("config.R", "helpers.R", "update.R", "backfill.R"))

test_that("every C2D4U_ variable a workflow sets is one its script reads", {
  u <- wf_lines("update.yml"); b <- wf_lines("backfill.yml")
  expect_setequal(setdiff(names(wf_env(wf_job(u, "update"))), update_reads()), character(0))
  for (job in c("enumerate", "fetch", "merge"))
    expect_setequal(setdiff(names(wf_env(wf_job(b, job))), backfill_reads()), character(0))
})

test_that("update.yml passes the deadline, the pool and reclassify, never the rebuild switch", {
  job <- wf_job(wf_lines("update.yml"), "update")
  env <- wf_env(job)
  expect_setequal(names(env), c("C2D4U_RECLASSIFY_ONLY", "C2D4U_POOL", "C2D4U_DEADLINE_MIN"))
  expect_identical(env[["C2D4U_DEADLINE_MIN"]], as.character(DEADLINE_MIN))
  expect_identical(DEADLINE_MIN_ENV, "C2D4U_DEADLINE_MIN")
  expect_identical(RECLASSIFY_ONLY_ENV, "C2D4U_RECLASSIFY_ONLY")
  expect_false(any(grepl(FORCE_REBUILD_ENV, job, fixed = TRUE)))
  # update_cli writes its marker into the directory the step passes it
  expect_true(any(grepl("Rscript scripts/update.R out/", job, fixed = TRUE)))
  expect_true(any(grepl("hashFiles('out/skip-publish') == ''", job, fixed = TRUE)))
  expect_true(any(grepl("if [ -f out/skip-publish ]; then", job, fixed = TRUE)))
})

test_that("backfill.yml's fetch and merge jobs agree on the shards and the roster directory", {
  b <- wf_lines("backfill.yml")
  fetch <- wf_job(b, "fetch"); merge <- wf_job(b, "merge")
  fe <- wf_env(fetch); me <- wf_env(merge)
  matrix_line <- grep("^\\s+shard: \\[", fetch, value = TRUE)
  shards <- as.integer(strsplit(gsub("^.*\\[|\\].*$", "", matrix_line), ",\\s*")[[1]])
  expect_identical(shards, seq_along(shards) - 1L)
  expect_identical(fe[["C2D4U_SHARD_N"]], as.character(length(shards)))
  expect_identical(me[["C2D4U_SHARD_N"]], fe[["C2D4U_SHARD_N"]])
  expect_identical(fe[["C2D4U_SHARD_I"]], "${{ matrix.shard }}")
  expect_false(is.na(suppressWarnings(as.numeric(fe[["C2D4U_RETRY_BUDGET_MIN"]]))))
  # C2D4U_ROSTER is the directory the roster artifact is downloaded into
  roster_dl <- "- uses: actions/download-artifact@v8"
  for (job in list(fetch, merge)) {
    k <- which(trimws(job) == roster_dl & grepl("name: roster", c(job[-1], "")))
    expect_length(k, 1L)
    expect_true(grepl(sprintf("path: %s }", wf_env(job)[["C2D4U_ROSTER"]]), job[k + 1L], fixed = TRUE))
  }
  expect_true(any(grepl(sprintf("with: { path: %s, pattern: shard-*", me[["C2D4U_PARTS"]]),
                        merge, fixed = TRUE)))
})

# Run backfill.R as the workflow does, from the repo root, with every proxy
# pointed at a closed port and a fake gh first on PATH that answers
# `release view` with "release not found".
run_backfill_cli <- function(mode, env) {
  bin <- withr::local_tempdir()
  writeLines(c("#!/bin/sh", 'echo "release not found" >&2', "exit 1"), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "0755")
  dead <- "http://127.0.0.1:9"
  env <- c(env, sprintf("%s=%s", c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY",
                                   "ALL_PROXY"), dead), "no_proxy=", "NO_PROXY=", "GH_TOKEN=",
           sprintf("PATH=%s:%s", bin, Sys.getenv("PATH")))
  withr::local_dir(.c2_root)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("scripts/backfill.R", mode),
                                  stdout = TRUE, stderr = TRUE, env = env, timeout = 120))
  list(output = out, status = as.integer(attr(out, "status") %||% 0L))
}

test_that("the merge CLI publishes from the variables backfill.yml's merge job sets", {
  me <- wf_env(wf_job(wf_lines("backfill.yml"), "merge"))
  N <- as.integer(me[["C2D4U_SHARD_N"]])
  d <- withr::local_tempdir()
  bins <- sprintf("r-cran-p%02d", seq_len(N + 2L))
  roster <- mk_roster(bins)
  rows <- rel_rows(bins, "1.0", "2024-02-01", seq_along(bins))
  mk_parts(file.path(d, "parts"), roster, rows, N)
  dir.create(file.path(d, "roster"))
  write_roster(file.path(d, "roster", ROSTER_FILE), roster)
  env <- c(sprintf("C2D4U_OUT=%s", file.path(d, "out")),
           sprintf("C2D4U_PARTS=%s", file.path(d, "parts")),
           sprintf("C2D4U_SHARD_N=%s", me[["C2D4U_SHARD_N"]]),
           sprintf("C2D4U_ROSTER=%s", file.path(d, "roster")))
  res <- run_backfill_cli("merge", env)
  expect_identical(res$status, 0L)
  m <- jsonlite::fromJSON(file.path(d, "out", "manifest.json"), simplifyVector = FALSE)
  expect_identical(m$history_method, HISTORY_METHOD)
  expect_identical(m$counted_through, "2026-07-04")
  expect_true(all(file.exists(file.path(d, "out", unlist(m$changed_shards)))))
  s <- out_summary(file.path(d, "out"))
  expect_identical(s$cnt_total[match(roster$package, s$package)], seq_along(bins))
})

test_that("the merge CLI stops without the shard count or the roster directory", {
  res <- run_backfill_cli("merge", c("C2D4U_SHARD_N=", "C2D4U_ROSTER="))
  expect_false(identical(res$status, 0L))
  expect_true(any(grepl("merge: set C2D4U_SHARD_N", res$output, fixed = TRUE)))
  res <- run_backfill_cli("merge", c("C2D4U_SHARD_N=12", "C2D4U_ROSTER="))
  expect_false(identical(res$status, 0L))
  expect_true(any(grepl("merge: set C2D4U_ROSTER", res$output, fixed = TRUE)))
})

test_that("both workflows share one Launchpad concurrency group and never cancel a run", {
  conc <- function(lines) {
    i <- grep("^\\s*concurrency:\\s*$", lines)
    expect_length(i, 1L)
    block <- lines[i + 1:2]
    c(group = sub("^\\s*group:\\s*", "", grep("group:", block, value = TRUE)),
      cancel = sub("^\\s*cancel-in-progress:\\s*", "", grep("cancel-in-progress:", block, value = TRUE)))
  }
  u <- wf_lines("update.yml"); b <- wf_lines("backfill.yml")
  cu <- conc(wf_job(u, "update"))
  cb <- conc(b[seq_len(grep("^jobs:", b) - 1L)])     # workflow level: spans every backfill job
  expect_identical(cu, c(group = "c2d4u-downloads-launchpad", cancel = "false"))
  expect_identical(cb, cu)
  expect_false(any(grepl("concurrency:", wf_job(u, "keepalive"))))
})

test_that("the update step times out after the fetch deadline and before its job does", {
  job <- wf_job(wf_lines("update.yml"), "update")
  steps <- wf_steps(job)
  run <- steps[[wf_step_at(steps, "Rscript scripts/update.R")]]
  step_limit <- wf_num(run, "timeout-minutes")
  job_limit <- wf_num(job[seq_len(grep("^    steps:", job) - 1L)], "timeout-minutes")
  expect_length(step_limit, 1L)
  expect_gt(step_limit, DEADLINE_MIN)
  expect_lt(step_limit, job_limit)
})

test_that("the workflows stay within Launchpad's connection throttle", {
  cap <- 24
  expect_lte(as.numeric(wf_env(wf_job(wf_lines("update.yml"), "update"))[["C2D4U_POOL"]]), cap)
  b <- wf_lines("backfill.yml")
  expect_lte(as.numeric(wf_env(wf_job(b, "enumerate"))[["C2D4U_POOL"]]), cap)   # a lone job
  fetch <- wf_job(b, "fetch")
  expect_lte(wf_num(fetch, "max-parallel") * as.numeric(wf_env(fetch)[["C2D4U_POOL"]]), cap)
})

test_that("each publisher keeps its out dir for a full cycle, then uploads what it rebuilt and the manifest last", {
  for (x in list(c("update.yml", "update"), c("backfill.yml", "merge"))) {
    steps <- wf_steps(wf_job(wf_lines(x[1]), x[2]))
    pub <- wf_step_at(steps, "gh release upload")
    art <- which(vapply(steps, function(s) any(grepl("actions/upload-artifact", s)) &&
                                            any(grepl("path: out/\\s*$", s)), logical(1)))
    expect_length(pub, 1L)
    expect_length(art, 1L)
    expect_lt(art, pub)
    # longer than the monthly cadence, so a failed publish can be completed
    # from it until the next run
    expect_gte(wf_num(steps[[art]], "retention-days"), 35)
    p <- steps[[pub]]
    expect_true(any(grepl(sprintf("out/%s", UPLOAD_LIST), p, fixed = TRUE)), info = x[1])
    expect_false(any(grepl("changed_shards", p, fixed = TRUE)), info = x[1])
    up <- grep("gh release upload", p)
    expect_true(grepl("out/manifest.json", p[max(up)], fixed = TRUE), info = x[1])
  }
})
