## Submission summary

This is the first submission of the `envar` package.

`envar` provides a unified interface to download, harmonise and extract a wide
range of environmental and socio-economic variables from established open data
sources for use in macroecology and biogeography.

## Test environments

* win-builder, R-devel (Windows Server 2022)
* win-builder, R-release 4.6.1 (Windows Server 2022)
* local: Windows 11, R 4.4.2
* local: Ubuntu 22.04, R 4.4.1

## R CMD check results

On win-builder (R-devel and R-release) `R CMD check` returns no ERRORs and no
WARNINGs, and a single NOTE:

* "New submission" — this is the first submission of the package.

The same NOTE lists four "possibly misspelled words" in the Description
(`biogeography`, `macroecology`, `reprojection`, `socio`). These are spelled
correctly: the first three are standard terms in the field, and "socio" is the
first half of the compound "socio-economic".

## Notes

* All examples that download data from remote services are wrapped in
  `\dontrun{}` because they require network access and large downloads from
  third-party hosts. The vignettes are pre-computed (the `*.Rmd.orig` pattern)
  so that no network access or large downloads occur during
  `R CMD build`/`check`.

* `rnaturalearthdata` is listed in `Suggests`: it is only needed to resolve
  study areas given by country or continent name, every such code path is
  guarded with `requireNamespace()`, and this mirrors how the upstream
  `rnaturalearth` package itself declares it.
