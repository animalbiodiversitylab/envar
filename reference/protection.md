# Download and process WDPA Protected Area layers

This function downloads, processes, and extracts variables from the
World Database of Protected Areas (WDPA). Each variable corresponds to a
global raster representing different IUCN Management Categories of
protected areas.

## Usage

``` r
protection(x, vars, ...)
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

**IUCN Management Categories**

- "WDPA_IA" ("strict nature reserve", "strict reserve", "1a", "ia",
  "Ia")

- "WDPA_IB" ("wilderness area", "wilderness", "1b", "ib", "Ib")

- "WDPA_II" ("national park", "park", "2", "ii", "II")

- "WDPA_III" ("natural monument", "monument", "3", "iii", "III")

- "WDPA_IV" ("habitat species management", "habitat management", "4",
  "iv", "IV")

- "WDPA_V" ("protected landscape", "protected seascape", "landscape",
  "5", "v", "V")

- "WDPA_VI" ("sustainable use", "natural resources", "6", "vi", "VI")

- "WDPA_ALL" ("all", "combined", "full", "total", "all protected areas")

**Citation:**  
Protected Planet (2025). "World Database of Protected Areas (WDPA)."
\[UNEP-WCMC\]. https://www.protectedplanet.net/en

Note: Users should ensure they comply with the terms of use of the WDPA
when using these data for commercial or research purposes.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Download National Parks and All Protected Areas for Italy
processed <- par_set(country= "Italy", crs=3035) %>% 
protection(vars=c("national park", "WDPA_ALL"))
  } # }
```
