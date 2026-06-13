# Download and process EarthEnv habitat heterogeneity layers

This function downloads, processes, and extracts variables from the
EarthEnv habitat heterogeneity dataset (1-km resolution). Each variable
corresponds to a global Cloud-Optimized GeoTIFF (COG) representing
different metrics of habitat heterogeneity derived from remote sensing
data.

## Usage

``` r
heterogeneity(x, vars, ...)
```

## Arguments

- x:

  The output from \`var_get()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`var_get()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- ...:

  Additional arguments (currently unused).

## Value

If \`var_get()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`var_get()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**First-order statistics**

- "cv" ("coefficient of variation", "coeff of variation")

- "evenness" ("even")

- "range"

- "shannon" ("shannon index", "shannon entropy")

- "simpson" ("simpson index", "simpson diversity")

- "std" ("standard deviation", "std dev")

**Second-order statistics (texture metrics)**

- "Contrast" ("contrast")

- "Correlation" ("correlation", "corr")

- "Dissimilarity" ("dissimilarity")

- "Entropy" ("entropy", "texture entropy")

- "Homogeneity" ("homogeneity")

- "Maximum" ("maximum", "max")

- "Uniformity" ("uniformity", "uniform")

- "Variance" ("variance", "var")

**Citation:**  
Tuanmu M-N, Jetz W (2015). "A global, remote sensing-based
characterization of terrestrial habitat heterogeneity for biodiversity
and ecosystem modeling." Global Ecology and Biogeography, 24, 1329-1339.
https://doi.org/10.1111/geb.12365

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Download Shannon Index and Coefficient of Variation for the Alps
processed <- var_get(shape = Alps, crs = 3035) %>% 
  heterogeneity(vars = c("shannon", "cv"))
  } # }
```
