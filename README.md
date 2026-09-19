# c2d4u-downloads

Per-package daily download counts for CRAN and Bioconductor R packages
distributed as Ubuntu `.deb` files through the Launchpad c2d4u ("cran2deb4ubuntu")
PPAs. Part of the r-observatory family of CRAN data pipelines.

> [!IMPORTANT]
> This is a frozen, legacy channel. The modern archive
> (`~c2d4u.team/c2d4u4.0+`) stopped publishing new binaries around February 2024
> and its own description points users to r2u, tracked separately by
> `r-observatory/r2u-downloads`. Counts here are apt pulls of cached debs: a
> declining historical signal, not a measure of current adoption. Data mixes
> CRAN and Bioconductor, distinguished by the `origin` column.

## Data

Published as SQLite assets on the rolling `current` GitHub release:

- `c2d4u-downloads-<year>.db`: per-year `c2d4u_downloads_daily(package, date, count)`.
- `c2d4u-downloads-recent.db`: the 400 days up to `counted_through` plus an embedded
  `c2d4u_downloads_summary` and the `c2d4u_releases` roster.
- `c2d4u-downloads-summary.db`: the summary table only.
- `manifest.json`: coverage and freshness metadata, including `counted_through`, `history_method`, `known_gaps`, `package_edges`, `refetch_from`, `package_refetch_from`, `unfetched_releases`, `empty_once_releases`, `coverage_scope`, `complete`, and a `sha256` for every database asset.

The roster's `done` column says how far each release's history is known:

- `0`: not fetched yet (or its fetch failed). Counted in `unfetched_releases`.
- `2`: fetched in full once, and Launchpad returned no downloads. One empty answer can be a transient one, so the next monthly update asks for the release's whole history again. Counted in `empty_once_releases`.
- `1`: settled. `last_day` is the release's last download, or empty when the release answered empty twice and so was never downloaded.

`package` is the lowercased, prefix-stripped token; `canonical_name` in the
summary restores the CRAN/Bioc case. `origin` is `cran`, `bioc`, or `other`.

## What the counts cover

- A package's count for a day is the sum of that day's downloads over every c2d4u release of it: every archive and every version.
- Only amd64 publications are counted, one per archive, binary and version. Downloads of the same version built for another architecture are not included; the manifest records this as `coverage_scope`.
- Launchpad reports a day's downloads within a day of it ending. A run counts through the day two days before its data was fetched and records that day as `counted_through`. The summary's 30, 90 and 365-day windows end on `counted_through`, not on the day you read them.
- Launchpad recorded no downloads for any publication from 2026-05-06 through 2026-07-11, and counts resume on 2026-07-12. The daily tables hold nothing for those days, and totals over a window that includes them are lower for it. The manifest lists the gap under `known_gaps`.
- If the monthly update finds no downloads anywhere for three or more of the last days it fetched, outside a known gap, it treats that as a stall in Launchpad rather than a quiet spell: `counted_through` stops before those days and the run logs a warning. A stall cannot last more than 90 days, counted from the last download of any release outside the known gaps: past that, downloads have stopped, so `counted_through` moves on through the quiet days, the run logs a warning, and the manifest lists the quiet stretch under `detected_gaps` with `kind` set to `"stopped"`. While that stretch is the latest gap and ends at `counted_through`, its first day is the refetch floor (`refetch_from`): every monthly run fetches it again from there, and so does the check of dormant releases once nothing is active. Downloads Launchpad fills in later are counted, and the gap shrinks to the days that are still empty; the next run still starts at the stretch's old first day, so the filled days are fetched in two runs like any newly counted day. A package held back in the run that finds them stores them only when a later run refreshes it, and fetches them again in the run after that. Once downloads come back after it, the stretch is an ordinary gap and is not fetched again.
- A package the monthly update could not refresh keeps its previous data and is listed in `package_edges` with the day its data stops. `complete` is true only when no package is listed there, no release is left unfetched, and no release awaits its second answer (`empty_once_releases`). A backfill leaves every release that answered empty in that state, so the release it publishes becomes complete only once the next monthly update has asked those releases again.

## Access

```sh
gh release download current --repo r-observatory/c2d4u-downloads \
  --pattern 'c2d4u-downloads-recent.db'
```

```r
con <- DBI::dbConnect(RSQLite::SQLite(), "c2d4u-downloads-recent.db")
DBI::dbGetQuery(con, "SELECT * FROM c2d4u_downloads_summary ORDER BY rank_30d LIMIT 20")
```

```python
import sqlite3, pandas as pd
con = sqlite3.connect("c2d4u-downloads-recent.db")
pd.read_sql("SELECT * FROM c2d4u_downloads_summary ORDER BY rank_30d LIMIT 20", con)
```

## How it is built

Counts come from Launchpad's anonymous REST API (`getPublishedBinaries` to
enumerate binaries, `getDownloadCounts` for per-day rows).

`backfill.yml`, dispatched by hand, builds the whole history. It enumerates every release in the enabled archives, fetches each release's full daily history across 12 sharded jobs, and sums every release's downloads per package and day. It refuses to publish when a shard's part is missing, when more than a tenth of the releases went unfetched, or when the result has fewer releases or packages than the published release, or fewer downloads for more than 1% of packages. It also refuses parts that count through an earlier day than the published release: they were fetched before that release was published (for example, a merge job re-run after a monthly update), so re-run the whole backfill, not just its merge job. It applies the monthly update's stall rule (see "What the counts cover") to the end of the history: a backfill fetched while Launchpad is stalled counts through the day before the stall, never earlier than the published `counted_through`, and one fetched after downloads stopped records the quiet stretch as stopped. It keeps the gaps the published release recorded, and judges a stretch that release counted as stopped again from its first day, as the monthly update does. A package the published release held keeps its refetch floor for the next monthly update, because the backfill can be the first run to store the days that floor covers.

`update.yml` runs monthly on the 3rd. For every release that had downloads in the year before its package's edge (`counted_through`, or the package's day in `package_edges`), it re-fetches the 30 days before that edge and everything since, which absorbs Launchpad's late revisions. Launchpad's counts only grow, so a re-fetched day that comes back lower than stored keeps its stored count. When a run counts more than 31 new days (after a month that published nothing, or when a held package catches up), the next run's window would start after the first of them, so the manifest records a refetch floor: `refetch_from` for every package, or a package's own day in `package_refetch_from`. The next run re-fetches from that day, so every newly counted day is fetched in two runs. A held package keeps its floor until it is refreshed and for one run after that, because that refresh can be the first to store days Launchpad filled in while the package was held. It also fetches the full history of any release not fetched yet, and asks again for the whole history of any release that answered empty once, as far as time allows; the rest wait for the next run. When no release is active any more, it first checks the dormant releases downloaded last for new downloads, and stops with a pointer to `backfill.yml` if they have any. It stops without publishing if it cannot request every release it re-fetches within its time limit, if Launchpad still fails for more than 2% of them after up to an hour of retrying them one at a time (fewer hold their packages back, see `package_edges`), or if Launchpad now reports fewer downloads than were stored for more than 10 packages or in total. A month with no newly settled day publishes nothing. The monthly update has no full-rebuild option; that is what `backfill.yml` is for.

Both publish a manifest with `history_method` set to `"summed"`, a `counted_through` day and a `sha256` for every database asset. The monthly update only builds on a release that carries all three and whose database assets match their hashes. A release from before the summed history makes it stop and point to `backfill.yml`. So does one whose publish failed partway, which is better completed from that publish's artifact (below).

## Running the workflows

- Both workflows share the concurrency group `c2d4u-downloads-launchpad`, so a backfill and a monthly update never fetch from Launchpad or publish at the same time: a run started while another is running waits as pending. GitHub keeps only one pending run per group, and a newly queued run replaces the pending one, which is cancelled without a failure notice. Do not dispatch either workflow while a run is pending. The monthly schedule's keepalive commit runs outside the group.
- `update.yml` dispatched with `reclassify_only` rebuilds identity and the summary from the published history without calling Launchpad.
- A publish uploads the database assets the run rebuilt, listed in `upload-list.txt`, one at a time, then `manifest.json` last. A heartbeat uploads only the manifest. `gh release upload --clobber` deletes an asset before it uploads the replacement, so a publish that fails partway can leave the release torn or missing an asset. The monthly update then stops, with "torn or foreign release" or an asset that could not be downloaded, until the release is repaired. Each run first saves what it is about to publish as a workflow artifact kept for 90 days (`update-out-attempt-<n>` or `backfill-out-attempt-<n>`). If the publish fails partway, and before any other run publishes, complete it from that artifact instead of fetching again:

```sh
gh run download <run-id> --repo r-observatory/c2d4u-downloads \
  --name update-out-attempt-1 --dir out
while IFS= read -r asset; do
  gh release upload current "out/$asset" --clobber --repo r-observatory/c2d4u-downloads < /dev/null
done < out/upload-list.txt
gh release upload current out/manifest.json --clobber --repo r-observatory/c2d4u-downloads
gh release edit current --notes-file out/release_notes.md --repo r-observatory/c2d4u-downloads
```

  `backfill.yml` can also replace such a release, unless the missing asset is the recent shard: the backfill reads the published roster from it, so complete the publish first.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
