## Resubmission

This is a resubmission. In response to the reviewer's comments we have made
the following changes:

* Added angle brackets around the web-service URLs in the Description field of
  the DESCRIPTION file (e.g. `<https://www.worldclim.org/>`), so that they are
  auto-linked, with no space after `http:`/`https:`.

* Each vignette now contains a short, self-contained executable code chunk (in
  a final "Appendix" section) that runs during `R CMD build`/`check`. Because
  the core purpose of the package is to download large environmental layers
  from remote services -- which requires network access and is unsuitable for
  automatic checks -- the download-based tutorials are pre-computed (the
  `*.Rmd.orig` pattern) and shown but not executed, while this final chunk runs
  the core functionality (collinearity and extrapolation checks, reprojection,
  aggregation, a simple model fit) end to end on a small dataset bundled with
  the package, with no network access.

* Replaced every `\dontrun{}` with `\donttest{}`. Examples that do not require
  a download (e.g. `corr_check()`, `extr_check()`) are now executable and run
  on the small bundled dataset; only the examples that download data from
  remote third-party services remain wrapped in `\donttest{}`.

* Removed all modifications of the global environment within functions (no
  `<<-` assignments remain).

## Submission summary

`envar` provides a unified interface to download, harmonise and extract a wide
range of environmental and socio-economic variables from established open data
sources for use in macroecology and biogeography.

## Test environments

* win-builder, R-devel (Windows Server 2022)
* win-builder, R-release 4.6.1 (Windows Server 2022)
* win-builder, R-oldrelease 4.5.3 (Windows Server 2022)
* local: Windows 11, R 4.4.2

## R CMD check results

On all three win-builder environments (R-devel, R-release 4.6.1 and
R-oldrelease 4.5.3) `R CMD check` returns no ERRORs and no WARNINGs, and a
single NOTE:

* "New submission" -- this is a new package.

The same NOTE lists four "possibly misspelled words" in the Description
(`biogeography`, `macroecology`, `reprojection`, `socio`). These are spelled
correctly: the first three are standard terms in the field, and "socio" is the
first half of the compound "socio-economic".

On R-oldrelease the incoming-feasibility check additionally flagged
`https://www.gbif.org` as a possibly invalid URL (HTTP 403); it did not appear
on R-devel or R-release. The URL is valid and opens normally in a browser; GBIF
returns 403 to the automated request made by the URL checker. It appears in the
`sdm` vignette and in the documentation of the bundled `Apollo` dataset, whose
source is GBIF.

The installed package size checked OK on all three win-builder environments. On
some other platforms it may be reported as slightly above 5 Mb (~7.7 Mb, mostly
`data/` and the pre-computed vignette figures under `doc/`); the datasets are
stored with `xz` compression (`LazyDataCompression: xz`).

## Notes

* Examples that download data from remote services are wrapped in `\donttest{}`
  because they require network access and large downloads from third-party
  hosts. The vignettes are pre-computed (the `*.Rmd.orig` pattern) so that no
  network access or large downloads occur during `R CMD build`/`check`; each
  vignette additionally includes one executable chunk that exercises the core
  functionality offline on a bundled dataset.

* `rnaturalearthdata` is listed in `Suggests`: it is only needed to resolve
  study areas given by country or continent name, every such code path is
  guarded with `requireNamespace()`, and this mirrors how the upstream
  `rnaturalearth` package itself declares it.
