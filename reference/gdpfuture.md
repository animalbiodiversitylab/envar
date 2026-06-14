# Download and process Future GDP Projections (SSP1-5)

This function downloads, processes, and extracts variables from the
global gridded GDP projections compatible with the five Shared
Socioeconomic Pathways (SSPs) for the period 1850–2100.

## Usage

``` r
gdpfuture(x, vars, year, ssp, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of variables to download (synonyms for "gdp").

- year:

  Numeric vector of years to download (decades, e.g., 2030, 2050).

- ssp:

  Numeric vector of SSP scenarios to download (values 1 to 5).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

- "gdp" ("gross domestic product", "future gdp", "economic projection",
  "economy", "ssp gdp")

**Scenarios (SSP)** The \`ssp\` argument accepts integers 1 through 5,
corresponding to:

- SSP1: Sustainability

- SSP2: Middle of the Road

- SSP3: Regional Rivalry

- SSP4: Inequality

- SSP5: Fossil-fueled Development

**Years available**

- Decadal intervals from 1850 to 2100 (e.g., 2020, 2030, 2040...).

**Citation:**  
Murakami D, Yoshida T, Yamagata Y (2021). "Gridded GDP projections
compatible with the five SSPs (shared socioeconomic pathways)."
Frontiers in Built Environment 7, 760306.
https://doi.org/10.3389/fbuil.2021.760306

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get GDP for SSP1 and SSP5 in 2050
processed <- par_set(country= "France", crs=3035) %>% 
  gdpfuture(vars="gdp", year=2050, ssp=c(1, 5))

# Get time series for SSP2
processed <- par_set(country= "India") %>% 
  gdpfuture(vars="gdp", year=c(2020, 2030, 2040), ssp=2)
} # }
```
