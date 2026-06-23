# Download and process Global Population Projections (SSP)

This function downloads, processes, and extracts variables from the
Global Population Projections dataset (Wang et al., 2022). It provides
1-km grid population distributions from 2020 to 2100 under five Shared
Socioeconomic Pathways (SSPs).

## Usage

``` r
population(x, vars, year = 2020, ssp = 1, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- year:

  Numeric vector. Years to download. Available from 2020 to 2100 in
  5-year intervals (e.g., c(2020, 2050)).

- ssp:

  Numeric vector. Shared Socioeconomic Pathways to download (1, 2, 3, 4,
  or 5).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

- 1 - "population" (\[People\]) ("pop", "inhabitants", "residents",
  "people", "count", "census")

**Citation:**  
Wang X, Meng X, Long Y (2022). "Projecting 1 km-grid population
distributions from 2020 to 2100 globally under shared socioeconomic
pathways." Scientific Data 9, 563.
[doi:10.1038/s41597-022-01675-x](https://doi.org/10.1038/s41597-022-01675-x)

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country = "Italy", crs = 3035) %>% 
  population(vars = "population", year = 2050, ssp = 2)
  } # }
```
