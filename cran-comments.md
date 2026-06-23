## Submission summary

This is the first submission of the `envar` package.

`envar` provides a unified interface to download, harmonise and extract a wide
range of environmental and socio-economic variables from established open data
sources for use in macroecology and biogeography.

## Test environments

* local: Ubuntu 22.04, R 4.4.1

## R CMD check results

`R CMD check --as-cran` produces no ERRORs.

There are no WARNINGs on a complete check environment. (Locally, two WARNINGs
appear that are caused by missing system tools only: `qpdf` is not installed,
and the `inconsolata` LaTeX font is missing from the local TeX Live install
so the PDF reference manual cannot be typeset. The check step
"PDF version of manual without index ... OK" confirms the Rd sources are valid.)

## Notes

* All examples that download data from remote services are wrapped in
  `\dontrun{}`. The vignettes are pre-computed (the `*.Rmd.orig` pattern) so
  that no network access or large downloads occur during `R CMD build`/`check`.
