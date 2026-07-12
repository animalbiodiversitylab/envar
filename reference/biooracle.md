# Download Bio-ORACLE marine data

This function downloads, processes, and extracts variables from the
Bio-ORACLE v3.0 dataset.

## Usage

``` r
biooracle(
  x,
  vars,
  realm = "surface",
  years = "2000-2010",
  ssp = NULL,
  algorithm = "mean",
  ...
)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations. It must
  have been created with \`res = 6\` (Bio-ORACLE's native 0.05-degree
  grid).

- vars:

  Character vector of one or more variables or synonyms to download.

- realm:

  Character. One of "surface" (default), "benthic_minimum",
  "benthic_average", or "benthic_maximum".

- years:

  Character. The time period for the data in "YYYY-YYYY" format. Use
  "2000-2010" or "2010-2020" for baseline current conditions (default is
  "2000-2010"). For future projections, specify the decade (e.g.,
  "2040-2050", "2090-2100") and provide the \`ssp\` argument.

- ssp:

  numeric or character. Shared Socioeconomic Pathway (119, 126, 245,
  370, 460, 585). Required if \`years\` is in the future (\>= 2020).

- algorithm:

  Character. Statistic to apply (max, mean, min, ltmax, ltmin, range).
  Default "mean".

- ...:

  Additional arguments.

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses and ""):

- 1 - "thetao" (Ocean temperature; \[ºC\]) ("temperature", "temp", "sea
  temperature")

- 2 - "so" (Salinity; \[-\]) ("sal", "salt", "saltiness")

- 3 - "sws" (Sea water velocity; \[m.s-1\]) ("velocity", "current
  speed", "speed")

- 4 - "swd" (Sea water direction; \[degree\]) ("direction", "current
  direction")

- 5 - "no3" (Nitrate; \[mmol . m-3\]) ("nitrate")

- 6 - "po4" (Phosphate; \[mmol . m-3\]) ("phosphate")

- 7 - "si" (Silicate; \[mmol . m-3\]) ("silicate", "silicon")

- 8 - "o2" (Dissolved molecular oxygen; \[mmol . m-3\]) ("oxygen",
  "dissolved oxygen", "o2")

- 9 - "dfe" (Iron; \[mmol . m-3\]) ("iron", "fe")

- 10 - "phyc" (Primary productivity; \[mmol . m-3\]) ("productivity",
  "pp", "primary production")

- 11 - "ph" (pH; \[-\]) ("acidity")

- 12 - "chl" (Chlorophyll; \[mg . m-3\]) ("chlorophyll", "chla")

- 13 - "sithick" (Sea ice thickness; \[m\]) ("ice thickness")

- 14 - "siconc" (Sea ice cover; \[Fraction\]) ("ice cover", "sea ice")

- 15 - "clt" (Cloud cover; \[

- 16 - "mlotst" (Mixed layer depth; \[m\]) ("mld", "mixed layer")

- 17 - "tas" (Air temperature; \[ºC\]) ("air temperature", "air temp")

- 18 - "par" (Photosynt. Avail. Radiation; \[E.m-2.day-1\]) ("light",
  "radiation")

- 19 - "kdpar" (Diffuse attenuation; \[m-1\]) ("attenuation",
  "turbidity")

- 20 - "bathymetry" (Bathymetry; \[m\]) ("depth", "elevation",
  "altitude")

- 21 - "slope" (Topographic slope; \[-\]) ("topographic slope")

- 22 - "aspect" (Topographic aspect; \[-\]) ("topographic aspect")

- 23 - "tpi" (Topographic position index; \[-\]) ("topographic position
  index")

- 24 - "tri" (Terrain ruggedness index; \[-\]) ("terrain ruggedness
  index", "ruggedness")

**Citation:**  
Assis J, Fernández Bejarano SJ, Salazar VW, Schepers L, Gouvêa L,
Fragkopoulou E, Leclercq F, Vanhoorne B, Tyberghein L, Serrão EA,
Verbruggen H, De Clerck O (2024). "Bio-ORACLE v3.0. Pushing marine data
layers to the CMIP6 Earth system models of climate change research."
Global Ecology and Biogeography, 33, e13813.
https://doi.org/10.1111/geb.13813

## Resolution

Bio-ORACLE layers are distributed on a 0.05-degree grid (~5.5 km at the
equator). Because `res` is a multiplier of the 30 arc-second base grid,
the value that reproduces this grid exactly is `res = 6` (\\6 \times
30''= 0.05^{\circ}\\). You must therefore call
[`par_set()`](https://animalbiodiversitylab.github.io/envar/reference/par_set.md)
with `res = 6`; any other value (including the default) raises an error.

## Examples

``` r
# \donttest{
# Example 1: Current conditions (Baseline)
current_env <- par_set(country = "Italy", crs = 3035, res = 6) %>%
  biooracle(vars = c("temperature", "salinity"),
            years = "2000-2010")

# Example 2: Future projections (2050, SSP 585)
future_env <- par_set(country = "Italy", crs = 3035, res = 6) %>%
  biooracle(vars = c("temperature", "salinity"),
            years = "2040-2050",
            ssp = 585)
  # }
```
