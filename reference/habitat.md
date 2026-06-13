# Download and process IUCN Habitat Classification layers

This function downloads, processes, and extracts variables from the IUCN
Global Habitat Classification Fractions dataset (Jung et al., 2020). The
data is available at Level 1 (broad) and Level 2 (detailed)
classifications.

## Usage

``` r
habitat(x, vars, level = 1, ...)
```

## Arguments

- x:

  The output from \`var_get()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`var_get()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- level:

  Integer. The classification level to download. 1 (broad) or 2
  (detailed). Defaults to 1.

- ...:

  Additional arguments (currently unused).

## Value

If \`var_get()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`var_get()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Level 1 (Broad Categories)**

- "100_Forest" (\[Fraction\]) ("forest", "100")

- "200_Savanna" (\[Fraction\]) ("savanna", "200")

- "300_Shrubland" (\[Fraction\]) ("shrubland", "300")

- "400_Grassland" (\[Fraction\]) ("grassland", "400")

- "500_Wetlands inland" (\[Fraction\]) ("wetlands inland", "wetlands",
  "inland wetlands", "500")

- "600_Rocky Areas" (\[Fraction\]) ("rocky areas", "rocky", "600")

- "800_Desert" (\[Fraction\]) ("desert", "800")

- "900_Marine - Neritic" (\[Fraction\]) ("marine neritic", "neritic",
  "900")

- "1000_Marine - Oceanic" (\[Fraction\]) ("marine oceanic", "oceanic",
  "1000")

- "1100_Marine - Deep Ocean Floor" (\[Fraction\]) ("marine deep ocean
  floor", "deep ocean floor", "1100")

- "1200_Marine - Intertidal" (\[Fraction\]) ("marine intertidal",
  "intertidal", "1200")

- "1400_Artificial - Terrestrial" (\[Fraction\]) ("artificial
  terrestrial", "artificial", "terrestrial artificial", "1400")

**Level 2 (Detailed Categories - Selection)**

- "101_Forest - Boreal" (\[Fraction\]) ("forest boreal", "boreal
  forest", "101")

- "104_Forest - Temperate" (\[Fraction\]) ("forest temperate",
  "temperate forest", "104")

- "105_Forest - Subtropical-tropical dry" (\[Fraction\]) ("dry forest",
  "tropical dry forest", "105")

- "106_Forest - Subtropical-tropical moist lowland" (\[Fraction\])
  ("moist lowland forest", "tropical moist forest", "106")

- "107_Forest - Subtropical-tropical mangrove vegetation" (\[Fraction\])
  ("mangrove", "mangroves", "107")

- "201_Savanna - Dry" (\[Fraction\]) ("dry savanna", "201")

- "303_Shrubland - Boreal" (\[Fraction\]) ("boreal shrubland", "303")

- "308_Shrubland - Mediterranean-type" (\[Fraction\]) ("mediterranean
  shrubland", "308")

- "401_Grassland - Tundra" (\[Fraction\]) ("tundra", "401")

- "1401_Arable Land" (\[Fraction\]) ("arable land", "cropland", "1401")

- "1405_Urban Areas" (\[Fraction\]) ("urban areas", "urban", "city",
  "1405")

- (See function code for full list of Level 2 variables)

**Citation:**  
Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
Rondinini C, Visconti P (2020). "A global map of terrestrial habitat
types." Scientific Data 7, 256.
https://doi.org/10.1038/s41597-020-00599-8

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Level 1 extraction (Forest and Artificial)
processed <- var_get(country = "Italy", crs = 3035) %>% 
  habitat(vars = c("Forest", "Artificial"), level = 1)

# Example 2: Level 2 extraction (Specific biomes)
processed_l2 <- var_get(country = "Brazil", crs = 3035) %>% 
  habitat(vars = c("Mangrove", "Tropical moist lowland forest"), level = 2)
  } # }
```
