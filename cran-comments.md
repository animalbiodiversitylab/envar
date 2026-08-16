## Summary

This is a patch release (0.1.0 -> 0.1.1) that fixes the ERROR and the NOTE
reported by the CRAN checks of version 0.1.0. We are sorry for submitting again
so soon after the previous update; the release is made only to correct the
check failures below.

### 1. ERROR in the examples (donttest flavour)

The `chelsa()` help page contained an example using `vars = "bio"`. That value
asks the user, in the console, which of the 19 bioclimatic variables to
download, so it cannot run in a non-interactive session and failed with
"No layers were successfully processed".

* The example has been removed from the documentation.
* In addition, `chelsa()` now checks `interactive()` before prompting and, in a
  non-interactive session, fails immediately with an informative message asking
  the user to name the variables explicitly (`vars = c("bio1", "bio12")`).

### 2. NOTE: new files in the user's home filespace (`~/.cache/R/envar`)

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

### 3. NOTE: new files in `~/tmp/scratch` (r-devel-linux-x86_64-debian-gcc)

The directories listed in this NOTE are the per-session temporary directories of
that check machine (`Rtmp*`) together with `xvfb-run.*` files, which the package
does not create. `envar` writes its temporary files only inside `tempdir()`,
which was double-checked for this release: every path is built with
`fs::path_temp()`/`tempdir()`, and no code outside the cache helper described
above touches any other location.

## Test environments

* local: Ubuntu 22.04, R 4.4.1 -- `R CMD check --as-cran`
* win-builder: Windows Server 2022, R 4.6.1 (R-release) -- 2 NOTEs
* macOS builder (mac.R-project.org), R-devel -- Status: OK (no NOTEs)

## R CMD check results

0 ERRORs, 0 WARNINGs, 3 NOTEs on the local check:

* "Days since last update: 5" -- this submission only fixes the check failures
  of 0.1.0 reported above.

* "installed size is 8.0Mb" (`data` 2.4Mb, `doc` 4.1Mb) -- the bundled datasets
  are stored with `xz` compression (`LazyDataCompression: xz`) and `doc`
  contains the pre-computed figures of the four vignettes.

* `https://www.gbif.org` reported as possibly invalid (HTTP 403). The URL is
  valid and opens normally in a browser; GBIF returns 403 to the automated
  request made by the URL checker. It appears in the `sdm` vignette and in the
  documentation of the bundled `Apollo` dataset, whose source is GBIF.

On win-builder the size and URL NOTEs do not appear, but a third one does:

* "Examples with CPU (user + system) or elapsed time > 10s": `corr_check`, at
  10.03s elapsed. The example runs entirely offline on the small raster bundled
  with the package (24 x 55 cells, 4 layers) and takes 0.7s locally and about 3s
  on the macOS builder, so it only marginally exceeds the threshold on that
  machine.

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
