# Download and process Historical Real GDP and Electricity Consumption

This function downloads, processes, and extracts variables from the
global 1km gridded revised real gross domestic product and electricity
consumption dataset (1992–2019).

## Usage

``` r
gdppast(x, vars, year, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of variables to download ("gdp" or "electricity").

- year:

  Numeric vector of years to download. Available range: 1992-2019.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Economic Metrics**

- "gdp" ("gross domestic product", "real gdp", "economy", "economic
  output", "gross product")

**Energy Metrics**

- "electricity" ("electricity consumption", "energy", "energy
  consumption", "power", "ec", "electric")

**Years available**

- 1992 to 2019.

**Citation:**  
Chen J, Gao M, Cheng S et al (2022). "Global 1 km x 1 km gridded revised
real gross domestic product and electricity consumption during 1992–2019
based on calibrated nighttime light data." Scientific Data 9, 202.
https://doi.org/10.1038/s41597-022-01322-5

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get GDP for 2000 and 2010
processed <- par_set(country= "Italy", crs=3035) %>% 
  gdppast(vars="gdp", year=c(2000, 2010))

# Get Electricity and GDP for 2019
processed <- par_set(country= "Vietnam") %>% 
  gdppast(vars=c("electricity", "gdp"), year=2019)
} # }
```
