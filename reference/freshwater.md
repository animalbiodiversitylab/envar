# Download and process EarthEnv Freshwater Environmental Variables

This function downloads, processes, and extracts variables from the
Near-global freshwater-specific environmental variables dataset. These
variables are available at 1 km resolution and capture upstream
catchment characteristics, including topography, land cover, soil, and
climate.

## Usage

``` r
freshwater(x, vars, year = NULL, month = NULL, algorithm = NULL, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- year:

  Numeric. Selected year(s) for extraction. Note that most EarthEnv
  freshwater variables are static layers (2015 version); this argument
  is primarily for consistency.

- month:

  Numeric. Selected month(s) (1-12) for extraction. Only applicable to
  monthly variables (e.g., tmin, tmax, prec).

- algorithm:

  Character. Aggregation method to filter specific bands.

  - For **Elevation** and **Slope**: "min" (Band 1), "max" (Band 2),
    "range" (Band 3), "avg" (Band 4).

  - For **Flow Accumulation**: "length" (Band 1), "acc" (Band 2).

  - For other variables: matches the string in the layer name.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Temperature**

- 1 - "monthly_tmin_average.nc" ("monthly minimum temperature average",
  "min temp average", "tmin avg", "tmin")

- 2 - "monthly_tmax_average.nc" ("monthly maximum temperature average",
  "max temp average", "tmax avg", "tmax")

- 3 - "monthly_tmin_weighted_average.nc" ("monthly minimum temperature
  weighted", "min temp weighted", "tmin weighted")

- 4 - "monthly_tmax_weighted_average.nc" ("monthly maximum temperature
  weighted", "max temp weighted", "tmax weighted")

**Precipitation**

- 5 - "monthly_prec_sum.nc" ("monthly upstream precipitation sum",
  "precipitation sum", "precip sum", "prec")

- 6 - "monthly_prec_weighted_sum.nc" ("monthly upstream precipitation
  weighted", "precipitation weighted", "precip weighted")

**Hydroclimatic**

- 7 - "hydroclim_average+sum.nc" ("hydroclimatic variables average",
  "hydroclim average", "hydroclim")

- 8 - "hydroclim_weighted_average+sum.nc" ("hydroclimatic variables
  weighted", "hydroclim weighted")

**Topography**

- 9 - "elevation.nc" ("upstream elevation", "elevation", "dem")

- 10 - "slope.nc" ("upstream slope", "slope")

- 11 - "flow_acc.nc" ("stream length", "flow accumulation", "flow")

**Land cover**

- 12 - "landcover_minimum.nc" ("upstream landcover minimum", "landcover
  min")

- 13 - "landcover_maximum.nc" ("upstream landcover maximum", "landcover
  max")

- 14 - "landcover_range.nc" ("upstream landcover range", "landcover
  range")

- 15 - "landcover_average.nc" ("upstream landcover average", "landcover
  avg", "landcover")

- 16 - "landcover_weighted_average.nc" ("upstream landcover weighted",
  "landcover weighted")

**Geology & Soil**

- 17 - "geology_weighted_sum.nc" ("upstream geology", "geology
  weighted", "geology")

- 18 - "soil_minimum.nc" ("upstream soil minimum", "soil min")

- 19 - "soil_maximum.nc" ("upstream soil maximum", "soil max")

- 20 - "soil_range.nc" ("upstream soil range", "soil range")

- 21 - "soil_average.nc" ("upstream soil average", "soil avg", "soil")

- 22 - "soil_weighted_average.nc" ("upstream soil weighted", "soil
  weighted")

**Quality control**

- 23 - "quality_control.nc" ("quality control", "qc")

**Citation:**  
Domisch S, Amatulli G, Jetz W (2015). "Near-global freshwater-specific
environmental variables for biodiversity analyses in 1 km resolution."
Scientific Data 2, 150073. https://doi.org/10.1038/sdata.2015.73

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
# \donttest{
# Topography with algorithm filtering (keeping only the average band)
processed <- par_set(country = "Switzerland", crs = 3035) %>% 
  freshwater(vars = c("elevation", "slope"), algorithm = "avg")

# Monthly Climate (January and July)
processed <- par_set(country = "Italy", crs = 3035) %>% 
  freshwater(vars = "tmin", month = c(1, 7))
# }
```
