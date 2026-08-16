# envar - measured time, RAM and disk per pipeline

Machine: Linux, 24 cores, 62 GB RAM, NVMe SSD, R 4.4.1, envar 0.1.1 (working tree),
terra defaults. Internet link measured at ~0.33 MB/s (~2.6 Mbit/s) while these
runs were made.

Every row is one `Rscript` process measured with `/usr/bin/time -v`
(see `README.md` for the method). **Runtimes are processing only**: the three
source files were already in the download cache, because download time is a
property of the user's bandwidth, not of the package. The cost of the download
is reported separately below.

## Runtime and RAM

The three sources are one layer each: CHELSA `tas` (2019, month 1), EarthEnv
`tpi`, MELC `tree`.

| # | Extent | Grid | Sources | Check | Runtime | Peak RAM | Peak scratch |
|---|---|---|---|---|---|---|---|
| ex01 | Switzerland | 538 x 233 (0.13 M cells) | 1 | – | 1.9 s | 256 MB | < 1 MB |
| ex02 | Switzerland | 538 x 233 | 2 | – | 2.1 s | 295 MB | < 1 MB |
| ex03 | Switzerland | 538 x 233 | 3 | – | 2.4 s | 274 MB | < 1 MB |
| ex04 | Switzerland | 538 x 233 | 3 | `corr_check()` | 2.9 s | 389 MB | < 1 MB |
| ex05 | Switzerland | 538 x 233 | 3 | `extr_check()` | 3.0 s | 277 MB | < 1 MB |
| ex06 | global | 43200 x 16800 (726 M cells) | 1 | – | 1.8 s | 240 MB | < 1 MB |
| ex07 | global | 43200 x 16800 | 2 | – | 29 s | 17.8 GB | 30 MB |
| ex08 | global | 43200 x 16800 | 3 | – | 75 s | 25.1 GB | 449 MB |
| ex09 | global | 43200 x 16800 | 3 | `corr_check()` | **failed** (killed at 112 s) | **> 55 GB** | 419 MB |
| ex10 | global | 43200 x 16800 | 3 | `extr_check()` | **failed** (killed at 111 s) | **> 55 GB** | 395 MB |
| ex11 | global, `res = 5` | 8640 x 3360 (29 M cells) | 3 | – | 23 s | 5.1 GB | < 1 MB |
| ex12 | global, `res = 5` | 8640 x 3360 | 3 | `corr_check()` | 29 s | 5.1 GB | < 1 MB |
| ex13 | global, `res = 5` | 8640 x 3360 | 3 | `extr_check()` | 115 s | 6.3 GB | < 1 MB |

`ex09`/`ex10` were run inside a cgroup with `MemoryMax` set first to 45 GB and
then to 55 GB; both times the R process was killed by the kernel (exit 137)
after having produced no result. A global 1 km pipeline with three layers can
therefore be *built* on a 62 GB machine, but neither check can be run on it as
it stands - hence the `res = 5` rows, which are the practical global option.

Baseline cost of an empty R session with terra loaded is ~240 MB, so the ~250 MB
rows are essentially free.

## Disk (SSD)

| What | Size |
|---|---|
| CHELSA `tas` 2019-01 (source file) | 147 MB |
| EarthEnv `tpi` (source file) | 292 MB |
| MELC `tree` (source file) | 395 MB |
| **1 source, downloaded once** | **147 MB** |
| **2 sources, downloaded once** | **439 MB** |
| **3 sources, downloaded once** | **834 MB** |
| Scratch (terra temporary rasters), global 1 km, 3 sources | 449 MB peak |
| Scratch, everything else | < 1 MB |
| Saved result, Switzerland, 3 layers (`writeRaster`) | 1 MB |
| Saved result, global `res = 5`, 3 layers | 124 MB |
| Saved result, global 1 km, 3 layers | ~3 GB (scaled from the `res = 5` file) |

Downloads are **independent of the extent**: the same global source file is
fetched for Switzerland as for the whole world; only the processing differs.
With `cache = TRUE` the 834 MB stay on disk and are reused; with `cache = FALSE`
they land in the session temporary directory and are freed when R exits, but are
re-downloaded on the next run.

## Cold vs warm cache

The global 3-source pipeline was also run with an empty cache:

| Run | Wall clock | Downloaded |
|---|---|---|
| cold cache | 2622 s (43.7 min) | 834 MB |
| warm cache | 75 s | 0 |

So ~97 % of a first run is download. At 0.33 MB/s (this machine) 834 MB takes
~42 min; on a 100 Mbit/s link the same download is ~70 s and the whole cold run
would be ~2.5 min.

## What drives each number

* **One source is nearly free at any extent.** With a single source there is
  nothing to align, so the layer is handed back as a reference to the file on
  disk: 1.8-1.9 s and ~250 MB whether the extent is Switzerland or the world.
* **RAM is driven by the global grid, not by the checks.** Each global 1 km
  layer is 726 M cells; held as doubles that is ~5.8 GB, and the observed cost is
  ~8 GB per layer once the alignment copies are counted (17.8 GB for two layers,
  25.1 GB for three).
* **`corr_check()` is the memory bottleneck.** It calls
  `terra::as.data.frame(x, na.rm = TRUE)` and then `stats::na.omit()`, i.e. it
  materialises the whole stack as a data.frame plus a second copy - on top of the
  25 GB the stack already costs. That is what exceeds 55 GB at 1 km.
* **`extr_check()` costs time rather than memory** once the grid is small enough
  to fit (115 s vs 23 s at `res = 5`): it evaluates the calibration envelope cell
  by cell over the whole raster.
* **At small extents nothing matters.** Every Switzerland combination, checks
  included, is ~2-3 s and under 400 MB.
