# Changelog

## envar (development version)

- [`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md)
  and
  [`worldclim()`](https://animalbiodiversitylab.github.io/envar/reference/worldclim.md)
  now take the SSP and the RCP as two separate arguments. For CMIP6
  projections they are combined into the scenario code internally, so
  e.g. `ssp = 5` together with `rcp = 8.5` downloads the `ssp585`
  scenario. A full code (e.g. `ssp = "585"`) is still accepted when
  `rcp` is left `NULL`.
- [`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md)
  monthly time-series downloads now use CHELSA’s current host and
  per-year folder layout, fixing downloads for months after June 2019
  (which the old URL structure no longer served) and extending the
  available range beyond 2019.
- [`biooracle()`](https://animalbiodiversitylab.github.io/envar/reference/biooracle.md)
  now requires `par_set(res = 5.5)` (Bio-ORACLE’s native ~0.05° / ~5.5
  km resolution) and aborts with a clear message for any other value,
  including the default.
  [`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
  now accepts fractional `res` multipliers to support this.
- Website favicons regenerated from the current package logo.
- Vignettes can now be pre-computed locally via `vignettes/precompute.R`
  (the `*.Rmd.orig` pattern), so figures and printed output produced by
  the long-running tutorials are baked into the rendered articles on the
  website.

## envar 0.1.0

- Initial creation of the package.
