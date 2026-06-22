# Changelog

## envar (development version)

- Much friendlier, more precise error messages for common mistakes:

  - [`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
    now validates `res` (must be a number \>= 1), `scale`
    (small/medium/large), `crs` (must be a CRS PROJ/sf understands), and
    `pointsdf` (must be points - polygons are pointed to `shape`
    instead), each reporting exactly what was supplied.
  - Mistyped ecological-boundary names (`ecoregion`, `biome`, `realm`,
    `country`, `continent`,
    marine/pelagic/mountain/glacier/zoogeographic regions, …) now abort
    with a “Did you mean …?” suggestion and either the full list of
    valid values or a pointer to it. Previously several of these
    (notably `ecoregion`/`biome`/`realm` and unknown
    countries/continents) failed silently or with a cryptic downstream
    error. Also fixed a bug where `mountain_region_cmec` looked up the
    wrong column.
  - [`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md)
    and
    [`worldclim()`](https://animalbiodiversitylab.github.io/envar/reference/worldclim.md)
    report exactly which of `gcm`/`ssp`/`rcp` is missing when a future
    period is requested, with a worked example.
  - [`worldclim()`](https://animalbiodiversitylab.github.io/envar/reference/worldclim.md)
    now rejects unknown variable names (with suggestions) instead of
    silently dropping them.

- The resolution guard in `process_raster_layer()` is no longer
  over-strict. Several datasets that are nominally “1 km” are
  distributed on a 0.01° grid (~1.11 km) — e.g. the Köppen-Geiger
  climate zones
  ([`climatezones()`](https://animalbiodiversitylab.github.io/envar/reference/climatezones.md))
  and the IUCN habitat fractions
  ([`habitat()`](https://animalbiodiversitylab.github.io/envar/reference/habitat.md))
  — and were being rejected at `res = 1`. The tolerance now allows a
  source up to 20% coarser than the requested resolution (still catching
  genuinely coarse sources), and the error message reports the source
  resolution and suggests a suitable `res`.

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
  now requires `par_set(res = 6)`, which reproduces Bio-ORACLE’s native
  0.05° (~5.5 km) grid exactly (6 × 30″ = 0.05°), and aborts with a
  clear message for any other value, including the default.
  [`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
  now also accepts fractional `res` multipliers.

- Website favicons regenerated from the current package logo.

- Vignettes can now be pre-computed locally via `vignettes/precompute.R`
  (the `*.Rmd.orig` pattern), so figures and printed output produced by
  the long-running tutorials are baked into the rendered articles on the
  website.

## envar 0.1.0

- Initial creation of the package.
