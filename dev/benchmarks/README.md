# envar benchmark examples

Ten example pipelines used to measure what an `envar` download-and-process run
costs in wall-clock time, RAM and disk. They vary along three axes:

* **extent** — small (Switzerland, `par_set(country = "Switzerland")`) vs global
  (`par_set()` with no study area);
* **number of sources** — one, two or three, one layer per source;
* **checks** — nothing, `corr_check()` or `extr_check()` on top of the three sources.

| script | extent | sources | check |
|---|---|---|---|
| `ex01_small_1src.R` | Switzerland | CHELSA | – |
| `ex02_small_2src.R` | Switzerland | CHELSA + topography | – |
| `ex03_small_3src.R` | Switzerland | CHELSA + topography + MELC | – |
| `ex04_small_3src_corr.R` | Switzerland | 3 | `corr_check()` |
| `ex05_small_3src_extr.R` | Switzerland | 3 | `extr_check()` |
| `ex06_global_1src.R` | global | CHELSA | – |
| `ex07_global_2src.R` | global | CHELSA + topography | – |
| `ex08_global_3src.R` | global | CHELSA + topography + MELC | – |
| `ex09_global_3src_corr.R` | global | 3 | `corr_check()` |
| `ex10_global_3src_extr.R` | global | 3 | `extr_check()` |
| `ex11_global5km_3src.R` | global, `res = 5` | CHELSA + topography + MELC | – |
| `ex12_global5km_3src_corr.R` | global, `res = 5` | 3 | `corr_check()` |
| `ex13_global5km_3src_extr.R` | global, `res = 5` | 3 | `extr_check()` |

`ex11`-`ex13` were added because the two global 1 km checks cannot be run on a
62 GB machine (see `RESULTS.md`).

The three sources are one layer each, all at the native ~1 km (30 arc-second)
grid and all in EPSG:4326 (`res = 1`, default CRS):

1. `chelsa(vars = "tas", years = 2019, months = 1)` — a single month of a single
   year of the CHELSA monthly time series (147 MB source file);
2. `topography(vars = "tpi")` — EarthEnv topographic position index (292 MB);
3. `melc(vars = "tree")` — MELC tree cover (395 MB).

## How the measurements were taken

Each script is run in a *separate* R process, so the numbers are not polluted by
what an earlier run left in memory:

```
TMPDIR=<fresh dir> /usr/bin/time -v Rscript exNN_....R
```

* **runtime** — `Elapsed (wall clock) time` from `/usr/bin/time -v`;
* **RAM** — `Maximum resident set size`, i.e. the peak RSS of the whole R
  process (baseline R + terra is ~240 MB of that);
* **scratch disk** — the size of the run's private `TMPDIR`, sampled every 2 s
  while the run is going, so it reflects the peak, not what is left at the end
  (terra writes its intermediate rasters there);
* **source downloads** — measured separately: the runs use `cache = TRUE` with an
  already-populated cache, so the reported runtimes are *processing only*.
  Download volume is the size of the source files; download time depends on the
  user's bandwidth, not on the package.

The runs use a private cache directory (`R_USER_CACHE_DIR` pointed at a scratch
folder) rather than the real user cache, so that a concurrent R session cannot
delete source files from under a running benchmark - which is exactly what
happened on the first attempt.

Results: see `RESULTS.md`.
