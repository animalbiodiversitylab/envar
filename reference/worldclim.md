# Download and process WorldClim Climate Data (Historical & Future)

This function downloads, processes, and extracts variables from the
WorldClim climate dataset. Each variable corresponds to a global raster
representing climate variables at approximately 1-km resolution. It
supports both Historical (v2.1, 1970-2000) and Future (CMIP6) data.

## Usage

``` r
worldclim(
  x,
  vars,
  years = NULL,
  months = NULL,
  gcm = NULL,
  rcp = NULL,
  ssp = NULL,
  ...
)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- years:

  Character vector of years or periods. For Historical: use "1970-2000"
  or "historical". For Future: use "2021-2040", "2041-2060",
  "2061-2080", "2081-2100".

- months:

  Numeric vector (1-12) specifying which months to download. Only
  applies to monthly historical variables. Ignored for bioclimatic
  variables, elevation, or future data.

- gcm:

  Character vector of General Circulation Models (for Future data).

- ssp:

  Character or numeric vector of Shared Socioeconomic Pathways (e.g.,
  "126", "585").

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Temperature**

- "tmin" ("min temp")

- "tmax" ("max temp")

- "tavg" ("average temp")

**Precipitation**

- "prec" ("precipitation", "pr")

**Physical**

- "srad" ("solar radiation")

- "wind" ("wind speed")

- "vapr" ("water vapor")

- "elev" ("elevation")

**Bioclimatic**

- "bio" (all 19 bioclimatic variables), or specific e.g., "bio1",
  "bio12"

**Citation:**  
Fick SE, Hijmans RJ (2017). "WorldClim 2: new 1-km spatial resolution
climate surfaces for global land areas." International Journal of
Climatology 37: 4302-4315. https://doi.org/10.1002/joc.5086

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country = "Italy", crs = 3035) %>% 
  worldclim(vars = c("tmin", "bio1"), years = "1970-2000")
  } # }
```
