# Changelog

## envar (development version)

- New
  [`metadata()`](https://animalbiodiversitylab.github.io/envar/reference/metadata.md)
  function. Added at the end of a pipeline, it writes a provenance
  summary of the whole download: the source URLs and the date each file
  was downloaded, the variables produced, the temporal period they refer
  to, the native resolution of every source dataset, the resolution, CRS
  and extent of the output, and the processing settings used (those of
  [`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
  and the arguments of every dataset function called). The summary is
  written as a readable report (`envar_metadata.txt`) and as a table
  (`envar_metadata.csv`) and is also returned with the pipeline object.
  As in
  [`corr_check()`](https://animalbiodiversitylab.github.io/envar/reference/corr_check.md),
  an interactive session asks at the console where to store the files,
  so a pipeline containing both functions asks once for each of them.

## envar 0.1.1

- [`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
  no longer writes to the user’s home filespace without permission. The
  `cache` argument now defaults to `NULL`, which asks once per
  interactive session whether the persistent download cache (in
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) may be
  used, and always answers “no” in non-interactive sessions, where a
  session temporary directory is used instead. Pass
  `cache = TRUE`/`FALSE`, or set `options(envar.cache = )`, to skip the
  question.

- [`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md)
  now fails with an informative message when `vars = "bio"` (which asks
  in the console which bioclimatic variables to download) is used in a
  non-interactive session, instead of failing later with an unrelated
  error. The corresponding example has been removed from the
  documentation.

- [`roads()`](https://animalbiodiversitylab.github.io/envar/reference/roads.md)
  gains the two aggregated variables `"primary"` (sum of road classes 4
  and 5) and `"other"` (sum of road classes 1, 2 and 3), alongside the
  existing `"all"` and the five single classes. Because `"primary"` now
  names the aggregated group, the single class 2 is requested with
  `"class2"` or `"primary class"`. The download links of the aggregated
  layers were updated.

- The
  [`roads()`](https://animalbiodiversitylab.github.io/envar/reference/roads.md)
  documentation and the “Available variables” article now describe how
  the road classes are derived from OpenStreetMap and what the
  aggregated layers contain.

- The land mask example in the “Package overview” article now uses a
  CHELSA climatology, which provides values over the sea, so that the
  effect of `land = TRUE` is actually visible.

## envar 0.1.0

CRAN release: 2026-07-31

- Initial creation of the package.
