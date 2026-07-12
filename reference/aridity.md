# Download and process Global Aridity Index and Potential Evapotranspiration (ET0)

This function downloads, processes, and extracts variables from the
Global Aridity Index and ET0 Database v3. Each variable corresponds to a
global raster representing aridity index or potential evapotranspiration
values.

## Usage

``` r
aridity(x, vars, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Annual Variables**

- "ai_v3_yr.tif" ("aridity index annual", "ai annual", "aridity annual",
  "ai year")

- "et0_v3_yr.tif" ("et0 annual", "potential evapotranspiration annual",
  "et0 year")

- "et0_v3_yr_sd.tif" ("et0 standard deviation", "et0 sd", "et0
  variability")

**Monthly Aridity Index**

- "ai_v3_01.tif" ... "ai_v3_12.tif" ("aridity index
  january"..."december", "ai jan"..."dec", "ai 01"..."12")

**Monthly Potential Evapotranspiration (ET0)**

- "et0_v3_01.tif" ... "et0_v3_12.tif" ("et0 january"..."december", "et0
  jan"..."dec", "et0 01"..."12")

**Citation:**  
Zomer RJ, Xu J, Trabucco A (2022). "Version 3 of the Global Aridity
Index and Potential Evapotranspiration Database." Scientific Data 9,
409. https://doi.org/10.1038/s41597-022-01493-1

Note: Data is downloaded from Figshare (Article ID 7504448).

## Examples

``` r
# \donttest{
processed <- par_set(country= "Italy", crs=3035) %>% 
aridity(vars=c("aridity index annual", "et0 january"))
  # }
```
