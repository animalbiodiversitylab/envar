# Download and process Harmonized World Soil Database v2.0

This function downloads, processes, and extracts variables from the
Harmonized World Soil Database v2.0 (HWSD v2.0). The variable
corresponds to a global raster file at 1 km resolution representing soil
types.

## Usage

``` r
soil(x, vars = NULL, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.
  Defaults to "hwsd" if left empty.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Soil Data**

- 1 - "hwsd" ("soil", "type", "soiltype", "soil type")

**Citation:**  
FAO, IIASA (2023). "Harmonized World Soil Database v2.0." Food and
Agriculture Organization of the United Nations, Rome and International
Institute for Applied Systems Analysis, Laxenburg, Austria.
https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country= "Italy", crs=3035) %>% 
soil(vars=c("soil"))
  } # }
```
