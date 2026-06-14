# Download and process Global 1 km Land Cover variables

This function downloads, processes, and extracts variables from the
Global 1 km Land Cover dataset. Each variable corresponds to a global
raster representing a specific land cover class or diversity index
derived from very high-resolution imagery.

## Usage

``` r
melc(x, vars, discover = TRUE, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- discover:

  Logical. If TRUE, creates a discovery map (unused in current
  implementation but kept for compatibility).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Land Cover Classes**

- 1 - "wetland" ("wetlands", "swamp", "marsh", "bog", "fen")

- 2 - "bare" ("bare ground", "bare soil", "desert", "unvegetated")

- 3 - "built" ("built area", "built up", "urban", "artificial",
  "impervious")

- 4 - "cropland" ("agriculture", "agricultural", "crop", "crops",
  "farming")

- 5 - "grass" ("grassland", "grass land", "meadow", "pasture",
  "prairie")

- 6 - "ice" ("snow", "snow and ice", "glacier", "ice", "permafrost")

- 8 - "mangrove" ("mangroves")

- 9 - "moss" ("mosses", "lichen", "lichens", "moss and lichen")

- 10 - "shrub" ("shrubland", "scrub", "bush", "thicket")

- 11 - "tree" ("trees", "forest", "woodland", "canopy", "canopy cover")

- 12 - "water" ("surface water", "lake", "river", "freshwater")

**Diversity & Metrics**

- 7 - "land_perc" ("percentage of land", "land percentage", "land cover
  fraction", "land fraction")

- 13 - "simpson" ("simpson index", "diversity simpson", "simpson
  diversity")

- 14 - "shannon" ("shannon index", "entropy", "shannon entropy",
  "shannon diversity")

- 15 - "evenness" ("evenness index", "pielou", "pielou evenness",
  "species evenness")

**Citation:**  
Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025). "Global 1 km
land cover for macroecological modelling from very high resolution
imagery." Figshare. https://doi.org/10.6084/m9.figshare.30665069

Note: Users should verify the terms of use provided at
https://figshare.com/s/4e7dee46628b530aee03

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country= "Italy", crs=3035) %>% 
  melc(vars=c("tree", "water"))
} # }
```
