# Download and process EarthEnv land cover variables

This function downloads, processes, and extracts variables from the
EarthEnv Consensus Land Cover dataset. Each variable corresponds to a
global raster representing a specific land cover class at 1-km
resolution.

## Usage

``` r
earthenvlandcover(x, vars, discover = TRUE, ...)
```

## Arguments

- x:

  The output from \`var_get()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`var_get()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- discover:

  Logical. If \`TRUE\` (default), downloads the version integrated with
  the DISCover dataset. If \`FALSE\`, downloads the version without
  DISCover integration.

- ...:

  Additional arguments (currently unused).

## Value

If \`var_get()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`var_get()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

- 1 - "consensus_full_class_1" ("evergreen deciduous needleleaf trees",
  "needleleaf trees", "needleleaf", "conifer")

- 2 - "consensus_full_class_2" ("evergreen broadleaf trees", "evergreen
  broadleaf", "broadleaf evergreen")

- 3 - "consensus_full_class_3" ("deciduous broadleaf trees", "deciduous
  broadleaf", "broadleaf deciduous")

- 4 - "consensus_full_class_4" ("mixed other trees", "mixed trees",
  "other trees", "mixed forest")

- 5 - "consensus_full_class_5" ("shrubs", "shrubland", "shrub")

- 6 - "consensus_full_class_6" ("herbaceous vegetation", "herbaceous",
  "grassland", "grass", "herbs")

- 7 - "consensus_full_class_7" ("cultivated and managed vegetation",
  "cultivated", "managed vegetation", "agriculture", "crops",
  "cropland")

- 8 - "consensus_full_class_8" ("regularly flooded vegetation", "flooded
  vegetation", "flooded", "wetland")

- 9 - "consensus_full_class_9" ("urban built up", "urban", "built up",
  "built-up", "artificial surface")

- 10 - "consensus_full_class_10" ("snow ice", "snow", "ice", "glacier",
  "permafrost")

- 11 - "consensus_full_class_11" ("barren", "barren land", "bare
  ground", "bare")

- 12 - "consensus_full_class_12" ("open water", "water", "water bodies")

**Citation:**  
Tuanmu MN, Jetz W (2014). "A global 1-km consensus land-cover product
for biodiversity and ecosystem modeling." Global Ecology and
Biogeography 23, 1031-1045. https://doi.org/10.1111/geb.12182

Note: Users should verify the terms of use for EarthEnv data provided at
https://www.earthenv.org/

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- var_get(country= "Italy", crs=3035) %>% 
earthenvlandcover(vars=c("snow ice"))
  } # }
```
