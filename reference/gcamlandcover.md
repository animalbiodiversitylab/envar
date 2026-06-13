# Download and process Global Future Land Use/Cover layers (2020-2100)

This function downloads, processes, and extracts simulated global land
use and land cover (LULC) data for the period 2020-2100.

## Usage

``` r
gcamlandcover(x, vars = "landcover", ssp = 126, year = 2020, ...)
```

## Arguments

- x:

  The output from \`var_get()\` defining the area or locations for
  extraction, the reference system, and the buffer.

- vars:

  Character. Currently unused/ignored as this function returns the
  landcover map defined by \`year\` and \`ssp\`, but kept for
  consistency. Default is "landcover".

- ssp:

  Numeric or Character. The SSP scenario code (126, 245, 370, 434, 585).
  Ignored if \`year\` is 2020.

- year:

  Numeric. The year of simulation (2020, 2030, 2050, 2070, 2100).

- ...:

  Additional arguments (currently unused).

## Value

If \`var_get()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack. If \`var_get()\` contained spatial points without
buffer: a \`data.frame\`.

## Details

The data represents 1 km resolution LULC maps. The original data is in
World Mercator projection and will be automatically reprojected to the
CRS defined in \`var_get()\`.

**Land cover codes**

- 1 - Cropland

- 2 - Forest

- 3 - Grassland

- 4 - Urban

- 5 - Barren

- 6 - Water

**Available Years**

- 2020, 2030, 2050, 2070, 2100

**Available SSPs (Shared Socioeconomic Pathways)**

- 126 (SSP1-2.6)

- 245 (SSP2-4.5)

- 370 (SSP3-7.0)

- 434 (SSP4-3.4)

- 585 (SSP5-8.5)

**Citation:**  
Zhang T, Cheng C, Wu X (2023). "Mapping the spatial heterogeneity of
global land use and land cover from 2020 to 2100 at a 1 km resolution."
Scientific Data 10, 748. https://doi.org/10.1038/s41597-023-02637-7

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Baseline (2020)
processed <- var_get(country= "Italy", crs=4326) %>% 
  gcamlandcover(year = 2020)

# Get Future (SSP5-8.5 in 2050)
processed <- var_get(country= "Italy", crs=4326) %>% 
  gcamlandcover(ssp = 585, year = 2050)
} # }
```
