# Download and process SPECTRE environmental threat layers

This function downloads, processes, and extracts variables from the
SPECTRE — Spatially Explicit ECosysTem ThREats dataset. Each variable
corresponds to a global Cloud-Optimized GeoTIFF (COG) representing a
different anthropogenic or climatic threat.

## Usage

``` r
spectre(x, vars, ...)
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

**Land Use and Human Pressure**

- 1 - "1_1_MINING_AREA_cog" ("mining area", "mining_area", "mining")

- 2 - "1_2_HAZARD_POTENTIAL_cog" ("hazard potential", "hazard")

- 3 - "1_3_HUMAN_DENSITY_cog" ("human density", "population", "pop")

- 4 - "1_4_BUILT_AREA_cog" ("built area", "built")

- 5 - "1_5_ROAD_DENSITY_cog" ("road density", "roads", "road")

- 6 - "1_6_FOOTPRINT_PERC_cog" ("human footprint", "footprint")

- 7 - "1_7_IMPACT_AREA_cog" ("impacted area", "impact area")

- 8 - "1_8_MODIF_AREA_cog" ("modified area", "modif area")

- 9 - "1_9_HUMAN_BIOMES_cog" ("human biomes", "biomes")

- 10 - "1_10_FIRE_OCCUR_cog" ("fires", "fire")

- 11 - "1_11_CROP_PERC_UNI_cog" ("crops uni", "crop uni", "crop")

- 12 - "1_12_CROP_PERC_IIASA_cog" ("crops iiasa", "iiasa crops")

- 13 - "1_13_LIVESTOCK_MASS_cog" ("livestock", "livestock mass")

**Forest Loss**

- 14 - "2_1_FOREST_LOSS_PERC_cog" ("forest loss")

- 15 - "2_2_FOREST_TREND_cog" ("forest trend")

**Light Pollution**

- 16 - "3_1_LIGHT_MCDM2_cog" ("light at night", "night light", "light")

**Climate Change**

- 17 - "5_1_TEMP_TRENDS_cog" ("temperature trends", "temp trends")

- 18 - "5_2_TEMP_SIGNIF_cog" ("temperature significance", "temp signif")

- 19 - "5_3_CLIM_EXTREME_cog" ("climate extremes")

- 20 - "5_4_CLIM_VELOCITY_cog" ("climate velocity", "velocity")

- 21 - "5_5_ARIDITY_TREND_cog" ("aridity trend", "aridity")

**Citation:**  
Branco VV, Capinha C, Rocha J, Correia L, Cardoso P (2024). "SPECTRE:
standardized global spatial data on terrestrial SPecies and ECosystems
ThREats." Global Ecology and Biogeography, 34, e13949.
<https://doi.org/10.1111/geb.13949>

Note: Many SPECTRE variables are derived from external primary datasets.
Users should consult and cite the original sources listed in the SPECTRE
supplementary materials.

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country= "Italy", crs=3035) %>% 
  spectre(vars=c("forest loss", "light at night"))
  } # }
```
