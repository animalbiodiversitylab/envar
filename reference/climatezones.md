# Download and process Köppen-Geiger climate classification maps

This function downloads, processes, and extracts variables from the
High-resolution (1 km) Köppen-Geiger maps dataset. Each variable
corresponds to a global GeoTIFF representing climate classification
zones based on historical data or future CMIP6 projections.

## Usage

``` r
climatezones(x, vars = "zones", years = "1991-2020", ssp = NULL, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector. Defaults to "zones". Accepted aliases include:
  "koppengeiger", "climate", "climatezones", "koppen".

- years:

  Character vector of time periods. Defaults to "1991-2020". Accepts
  formats with underscores or hyphens (e.g., "1901-1930" or
  "1901_1930").

- ssp:

  Numeric or character vector of Shared Socioeconomic Pathways. Required
  for future projections (e.g., 126, 585).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

- 1 - "zones" ("koppengeiger", "climate", "climatezones", "koppen",
  "koppen geiger")

**Time Periods** (years argument):

**Historical**

- "1901-1930"

- "1931-1960"

- "1961-1990"

- "1991-2020" (default)

**Future**

- "2041-2070"

- "2071-2099"

**SSP Scenarios** (ssp argument, required for future periods):

- 119 (SSP1-1.9)

- 126 (SSP1-2.6)

- 245 (SSP2-4.5)

- 370 (SSP3-7.0)

- 434 (SSP4-3.4)

- 460 (SSP4-6.0)

- 585 (SSP5-8.5)

**Citation:**  
Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z,
Jiang X, van Dijk AIJM, Miralles DG (2023). "High-resolution (1 km)
Köppen-Geiger maps for 1901-2099 based on constrained CMIP6
projections." Scientific Data 10, 724.
https://doi.org/10.1038/s41597-023-02549-6

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country= "Italy", crs=3035) %>% 
climatezones(vars="zones", years="1991-2020")
  } # }
```
