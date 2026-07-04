# c2d4u-downloads

Per-package daily download counts for CRAN and Bioconductor R packages
distributed as Ubuntu `.deb` files through the Launchpad c2d4u ("cran2deb4ubuntu")
PPAs. Part of the r-observatory family of CRAN data pipelines.

> [!IMPORTANT]
> This is a frozen, legacy channel. The modern archive
> (`~c2d4u.team/c2d4u4.0+`) stopped publishing new binaries around February 2024
> and its own description points users to r2u, tracked separately by
> `r-observatory/r2u-downloads`. Counts here are apt pulls of cached debs: a
> declining historical signal, not a measure of current adoption. Launchpad's
> reported day also lags wall-clock by roughly 60 days. Data mixes CRAN and
> Bioconductor, distinguished by the `origin` column.

## Data

Published as SQLite assets on the rolling `current` GitHub release:

- `c2d4u-downloads-<year>.db`: per-year `c2d4u_downloads_daily(package, date, count)`.
- `c2d4u-downloads-recent.db`: the last 400 days plus an embedded
  `c2d4u_downloads_summary` and the `c2d4u_releases` roster.
- `c2d4u-downloads-summary.db`: the summary table only.
- `manifest.json`: coverage and freshness metadata.

`package` is the lowercased, prefix-stripped token; `canonical_name` in the
summary restores the CRAN/Bioc case. `origin` is `cran`, `bioc`, or `other`.

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
enumerate binaries, `getDownloadCounts` for per-day rows). The one-time bootstrap
runs as a sharded GitHub Actions job (`backfill.yml`); a monthly job
(`update.yml`) re-fetches the still-active tail of releases and rebuilds the
changed shards.
