# envar 0.1.1

* New `metadata()` function. Added at the end of a pipeline, it writes a
  provenance summary of the whole download: the source URLs and the date each
  file was downloaded, the variables produced, the temporal period they refer
  to, the native resolution of every source dataset, the resolution, CRS and
  extent of the output, and the processing settings used (those of `par_set()`
  and the arguments of every dataset function called). The summary is written as
  a readable report (`envar_metadata.txt`) and as a table (`envar_metadata.csv`)
  and is also returned with the pipeline object. As in `corr_check()`, an
  interactive session asks at the console where to store the files, so a
  pipeline containing both functions asks once for each of them.

* The correlation plot written by `corr_check()` is no longer a fixed
  2000 x 2000 pixel image: its side now grows with the number of variables,
  from 1200 up to 2000 pixels at 300 dpi, so that small sets of variables are
  not drawn on a mostly empty sheet.

* `par_set()` no longer writes to the user's home filespace without permission.
  The `cache` argument now defaults to `NULL`, which asks once per interactive
  session whether the persistent download cache (in `tools::R_user_dir()`) may
  be used, and always answers "no" in non-interactive sessions, where a session
  temporary directory is used instead. Pass `cache = TRUE`/`FALSE`, or set
  `options(envar.cache = )`, to skip the question.

* `chelsa()` now fails with an informative message when `vars = "bio"` (which
  asks in the console which bioclimatic variables to download) is used in a
  non-interactive session, instead of failing later with an unrelated error.
  The corresponding example has been removed from the documentation.

* `roads()` gains the two aggregated variables `"primary"` (sum of road classes
  4 and 5) and `"other"` (sum of road classes 1, 2 and 3), alongside the
  existing `"all"` and the five single classes. Because `"primary"` now names
  the aggregated group, the single class 2 is requested with `"class2"` or
  `"primary class"`. The download links of the aggregated layers were updated.

* The `roads()` documentation and the "Available variables" article now
  describe how the road classes are derived from OpenStreetMap and what the
  aggregated layers contain.

* The land mask example in the "Package overview" article now uses a CHELSA
  climatology, which provides values over the sea, so that the effect of
  `land = TRUE` is actually visible.

# envar 0.1.0

* Initial creation of the package.
