# envar 0.1.1

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
