## Summary

This is a patch release (0.1.0 -> 0.1.1). It fixes the ERROR and the NOTE
reported by the CRAN checks of version 0.1.0, and it also addresses the
remark made during the previous review ("keep the 5 sec threshold in mind for
future submissions"): no example of the package now takes anywhere near that
long.

### 1. Example timings (the 5 second threshold)

The check of 0.1.0 on r-devel-linux-x86_64-debian-gcc listed

    Examples with CPU (user + system) or elapsed time > 5s
                 user system elapsed
    corr_check  7.737  0.594   9.936

Profiling showed that almost all of that time was the lazy loading of the
`usdm` namespace (1.8s of the 1.9s measured here; `usdm` loads `raster` and
`sp` in turn), which `R CMD check` charges to the first example that calls it.
Three changes were made:

* `terra` and `usdm` are now imported in `NAMESPACE`, so they are loaded when
  the package is loaded rather than inside the first example that needs them;
* the runnable part of the `corr_check()` example is a single call on a small
  data frame (30 rows, 3 columns); the demonstration on the bundled
  `SpatRaster` moved into the `\donttest{}` block with the other pipelines;
* `corr_check()` no longer always renders a fixed 2000 x 2000 pixel
  correlation plot. The side of the (square) image now grows with the number
  of variables, from 1200 up to 2000 pixels at 300 dpi, which is both faster
  and better looking for small sets of variables.

The three examples that run outside `\donttest{}` now take, on the machine
where the 0.1.0 example took 2.3s:

    corr_check  0.095  0.006  0.092
    extr_check  0.213  0.004  0.217
    metadata    0.029  0.001  0.029

### 2. ERROR in the examples (donttest flavour)

The `chelsa()` help page contained an example using `vars = "bio"`. That value
asks the user, in the console, which of the 19 bioclimatic variables to
download, so it cannot run in a non-interactive session and failed with
"No layers were successfully processed".

* The example has been removed from the documentation.
* In addition, `chelsa()` now checks `interactive()` before prompting and, in a
  non-interactive session, fails immediately with an informative message asking
  the user to name the variables explicitly (`vars = c("bio1", "bio12")`).

### 3. NOTE: new files in the user's home filespace (`~/.cache/R/envar`)

Version 0.1.0 stored the files it downloads in the per-user cache directory
returned by `tools::R_user_dir()` by default, so running the examples created
`~/.cache/R/envar/grids`.

Nothing is now written outside the session temporary directory unless the user
has agreed to it:

* the `cache` argument of `par_set()` defaults to `NULL`, which means "ask";
* in an interactive session the user is asked once per session whether the
  persistent cache may be used, and the answer is remembered for that session;
* in a **non-interactive** session (scripts, `R CMD check`, vignette building)
  the answer is always "no" and a session temporary directory is used, so no
  file is ever created in the user's home filespace;
* `cache = TRUE`/`cache = FALSE`, or `options(envar.cache = )`, can be used to
  answer explicitly and skip the question.

This was verified by running a download non-interactively with `HOME` pointed at
an empty directory: nothing at all was created under `HOME`.

### 4. Installed size and URLs

* The figures of the four pre-computed vignettes and the images under
  `man/figures` have been recompressed as palette PNGs, which reduces the
  installed size of the package from 7.7 Mb to 5.1 Mb (`doc` from 3.9 Mb to
  1.8 Mb) and the source tarball from 8.8 Mb to 5.0 Mb.

* `https://www.gbif.org`, reported as possibly invalid (HTTP 403) by the URL
  checker of the previous submission, is no longer used as a link. GBIF is
  still credited in plain text in the `sdm` vignette and in the documentation
  of the bundled `Apollo` dataset (`@source GBIF`).

## Test environments

* local: Ubuntu 22.04, R 4.4.1 -- `R CMD check --as-cran`
* win-builder: Windows Server 2022, R-release
* macOS builder (mac.R-project.org), R-devel

## R CMD check results

0 ERRORs, 0 WARNINGs on the local check. The only NOTE left is

* "installed size is 5.1Mb" (`data` 2.4Mb, `doc` 1.8Mb) -- the bundled
  datasets are already stored with `xz` compression
  (`LazyDataCompression: xz`) and `doc` contains the pre-computed figures of
  the four vignettes, now stored as palette PNGs.

## Notes

* Examples that download data from remote services remain wrapped in
  `\donttest{}` because they require network access and large downloads from
  third-party hosts. The vignettes are pre-computed (the `*.Rmd.orig` pattern)
  so that no network access or large download occurs during `R CMD
  build`/`check`; each vignette additionally includes one executable chunk that
  exercises the core functionality offline on a bundled dataset.

* `rnaturalearthdata` is listed in `Suggests`: it is only needed to resolve
  study areas given by country or continent name, every such code path is
  guarded with `requireNamespace()`, and this mirrors how the upstream
  `rnaturalearth` package itself declares it.
