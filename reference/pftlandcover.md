# Download and process Global PFT Land Cover Projections (SSPs-RCPs)

This function downloads, processes, and extracts land cover variables
from the Global 7-land-types LULC projection dataset based on plant
functional types (PFT) with a 1-km resolution under socio-climatic
scenarios (Chen et al., 2022).

## Usage

``` r
pftlandcover(x, vars = NULL, year = 2025, ssp = 585, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- year:

  Integer. The year of the projection (2020-2100, step 5). Defaults to
  2025.

- ssp:

  Integer or Character. The SSP-RCP scenario code (119, 126, 245, 370,
  434, 460, 534, 585). Defaults to 585.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Land Cover**

- 1 - "landcover" ("cover", "land", "lulc", "pft", "projection")

**Citation:**  
Chen G, Li X, Liu X (2022). "Global land projection based on plant
functional types with a 1-km resolution under socio-climatic scenarios."
Scientific Data 9, 125. https://doi.org/10.1038/s41597-022-01208-6

Note: If the \`vars\` argument is left empty, the function will default
to downloading the land cover map.

## Examples

``` r
# \donttest{
# Download SSP5-RCP8.5 projection for 2050
processed <- par_set(country= "Italy", crs=3035) %>% 
  pftlandcover(vars="landcover", year=2050, ssp=585)
  # }
```
