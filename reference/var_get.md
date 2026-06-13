# Initialize the Environmental Variable Retrieval Pipeline

`var_get()` is the entry point for the **envar** package workflow. It
defines the spatial extent, resolution, and coordinate reference system
(CRS) for the study area.

## Usage

``` r
var_get(
  country = NULL,
  continent = NULL,
  shape = NULL,
  ecoregion = NULL,
  biome = NULL,
  realm = NULL,
  zooregion = NULL,
  zoorealm = NULL,
  mountain_region = NULL,
  mountain_region_cmec = NULL,
  glacier_region_19 = NULL,
  glacier_region_20 = NULL,
  freshwater_ecoregion = NULL,
  marine_ecoregion = NULL,
  marine_realm = NULL,
  marine_province = NULL,
  pelagic_province = NULL,
  pelagic_biome = NULL,
  pelagic_realm = NULL,
  pointsdf = NULL,
  alpha_hull = FALSE,
  buffer = 0,
  res = NULL,
  path = NULL,
  crs = "EPSG:4326",
  set_na = FALSE,
  scale = "medium",
  land = FALSE,
  cache = TRUE
)
```

## Arguments

- country:

  Character. The English name of a country (e.g., `"Italy"`,
  `"Viet Nam"`). Used to generate the extent if `shape` is `NULL`.

- continent:

  Character. The English name of a continent (e.g., `"Europe"`,
  `"Africa"`). Used to generate the extent if `shape` and `country` are
  `NULL`.

- shape:

  An `sf` object representing the study area. This can be:

  - **Polygons:** defining a region of interest.

  - **Points:** defining specific sampling locations.

  If `shape` is provided, `country` and `continent` are ignored.

- ecoregion:

  Character. The name of a terrestrial ecoregion from Dinerstein et al.
  (2017). Uses the `ECO_NAME` column from the Ecoregions2017 dataset.

- biome:

  Character. The name of a biome from Dinerstein et al. (2017). Uses the
  `BIOME_NAME` column from the Ecoregions2017 dataset.

- realm:

  Character. The name of a biogeographic realm from Dinerstein et al.
  (2017). Uses the `REALM` column from the Ecoregions2017 dataset.

- zooregion:

  Character. The name of a zoogeographic region from Holt et al. (2013).
  Uses the `Regions` column from the CMEC dataset.

- zoorealm:

  Character. The name of a zoogeographic realm from Holt et al. (2013).
  Uses the `Realm` column from the CMEC newRealms dataset.

- mountain_region:

  Character. The name of a mountain region from the GMBA Mountain
  Inventory v2.0 (Snethlage et al. 2022). Uses the `MapName` column.

- mountain_region_cmec:

  Character. The name of a mountain region from the Center for
  Macroecology, Evolution, and Climate definition of mountain areas
  (Rahbek et al. 2019). Uses the `Name` column.

- glacier_region_19:

  Character. The name of a glacier region based on RGI v6.0 (2017)
  first-order regions. Uses the `RGI_CODE` column.

- glacier_region_20:

  Character. The name of a glacier region based on GTN-G 2023
  first-order regions. Uses the `o1region` column.

- freshwater_ecoregion:

  Character or Numeric. The `FEOW_ID` of a freshwater ecoregion from
  Abell et al. (2008). Uses the `FEOW_ID` column from the FEOW dataset.

- marine_ecoregion:

  Character. The name of a marine ecoregion from Spalding et al. (2007).
  Uses the `ECOREGION` column from the MEOW dataset (`TYPE == "MEOW"`).

- marine_realm:

  Character. The name of a marine realm from Spalding et al. (2007).
  Uses the `REALM` column from the MEOW dataset (`TYPE == "MEOW"`).

- marine_province:

  Character. The name of a marine province from Spalding et al. (2007).
  Uses the `PROVINC` column from the MEOW dataset (`TYPE == "MEOW"`).

- pelagic_province:

  Character. The name of a pelagic province from Spalding et al. (2012).
  Uses the `PROVINC` column from the PPOW dataset (`TYPE == "PPOW"`).

- pelagic_biome:

  Character. The name of a pelagic biome from Spalding et al. (2012).
  Uses the `BIOME` column from the PPOW dataset (`TYPE == "PPOW"`).

- pelagic_realm:

  Character. The name of a pelagic realm from Spalding et al. (2012).
  Uses the `REALM` column from the PPOW dataset (`TYPE == "PPOW"`).

- pointsdf:

  Data.frame with columns `X` and `Y` representing point coordinates.

- alpha_hull:

  Logical. If `TRUE`, creates an alpha hull polygon around the
  occurrence points using the `getDynamicAlphaHull` function from the
  rangeBuilder package (Rabosky et al. 2016) to model species
  distribution ranges. The `initialAlpha` is set to 2 and gradually
  increased until a polygon captures at least 99% of occurrence records.
  This method optimizes the balance between identifying distinct
  clusters as unique polygons and avoiding excessive fragmentation (Roll
  et al. 2017). Can be used in conjunction with `buffer` (applied after
  alpha hull creation) and `land` (intersects result with land
  boundary). Default is `FALSE`.

  References:

  - Rabosky ARD, et al. (2016). BAMMtools: an R package for the analysis
    of evolutionary dynamics on phylogenetic trees. Methods in Ecology
    and Evolution 7:701-707.

  - Roll U, et al. (2017). The global distribution of tetrapods reveals
    a need for targeted reptile conservation. Nature Ecology & Evolution
    1:1677-1682.

- buffer:

  Numeric. A buffer distance in **kilometers** to expand or shrink the
  extent. The buffer is always specified in kilometers regardless of the
  target CRS - the function automatically converts to the appropriate
  units internally (degrees for geographic CRS like EPSG:4326, meters
  for projected CRS like EPSG:3035 or ESRI:54009).

  - **Positive values**: Expand the area outward by this distance.

  - **Negative values**: Shrink the area inward by this distance (useful
    for excluding coastal/border areas where data may have different
    characteristics).

  - For **points with positive buffer**: A circular buffer of this
    radius is drawn around each point, effectively converting the study
    area into polygons.

  - Default is `0`.

- res:

  Numeric. The target spatial resolution multiplier.

  - This controls the cell size of the output raster stack.

  - Must be a positive integer (e.g., `1`, `5`, `10`).

  - Default is `1` (30 arc-seconds or 0.008333333° at the equator).

  - Higher values will multiply the original 30 arcsec resolution by the
    specified factor.

- path:

  directory to store the result of the download/processing. Default to
  `NULL` (no output is stored locally). It works only if no
  [`corr_check()`](https://animalbiodiversitylab.github.io/envar/reference/corr_check.md)
  is specified. Specify the path including the file name and the
  extension (e.g. `"../Out/rastername.tif"` if the final export is a
  `SpatRaster`; or `"../Out/extracteddataframe.csv"` if the output is a
  `data.frame`).

- crs:

  Character or Numeric. The Coordinate Reference System for the **final
  output**.

  - Can be an EPSG code with or without prefix (e.g., `4326`, `3035`,
    `"EPSG:4326"`), an ESRI code (e.g., `54009`, `"ESRI:54009"`), a
    PROJ4 string, or WKT.

  - If `NULL`, the pipeline uses the standard default WGS84
    (`EPSG:4326`).

  - If specified, all downstream environmental layers will be projected
    to this CRS after processing.

  - Note: ESRI codes (53000-54999, 100000+) are automatically recognized
    and prefixed with "ESRI:" internally.

- set_na:

  Logical, with default `FALSE`. If `TRUE`, any cell that is `NA` in at
  least one raster is set to be `NA` in all rasters of the final
  `SpatRaster` object. It is useful only when the output is a
  `SpatRaster` and not a point extraction.

- scale:

  Character with value `"small"`, `"medium"`, or `"large"`. It
  represents the scale at which the country/continent shapefile are
  retrieved using the rnaturalearthdata package. Large implies a better
  definition of the borders of the shapefile (scale 1:10). The default
  is `"medium"`. It is useful only when setting the argument `country`
  or `continent`.

- land:

  Logical, with default `FALSE`. If `TRUE`, the extent is intersected
  with the global land boundary from Natural Earth (at the scale defined
  by the `scale` argument). This is useful for clipping marine/pelagic
  regions to land only, or for ensuring that buffered areas do not
  extend into the ocean. Note: This does not apply to point extractions
  (`pointsdf` without buffer).

- cache:

  Logical, with default `TRUE`. If `TRUE`, each source file downloaded
  by the downstream functions (e.g.
  [`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md),
  [`worldclim()`](https://animalbiodiversitylab.github.io/envar/reference/worldclim.md),
  [`topography()`](https://animalbiodiversitylab.github.io/envar/reference/topography.md))
  is stored in a persistent per-user cache directory. If the download
  pipeline is interrupted (for example by a lost connection) and then
  re-launched, it resumes from where it stopped, reusing files that were
  already retrieved instead of downloading them again. Set to `FALSE` to
  use a temporary directory that is cleared at the end of the R session.
  The cache can be emptied at any time with
  [`clear_cache`](https://animalbiodiversitylab.github.io/envar/reference/clear_cache.md).

## Value

A `list` object (class `envar_par`) containing:

- `grid`: A template `SpatRaster` defining the resolution and extent
  (for polygon input).

- `mask`: An `sf` object defining the exact study area boundaries (for
  polygon input).

- `res`: The resolution multiplier used.

- `bbox`: The bounding box of the study area.

- `crs`: The target coordinate reference system.

- `type`: The type of input (`"polygon"`, `"admin"`, or `"point"`).

- `is_global`: Logical, `TRUE` if processing global extent.

- `set_na`: Logical, `TRUE` if user wants to apply an NA mask.

- `path`: User-specified path to store the result.

## Details

This function does not download data itself. Instead, it creates a
standardized spatial template (grid) or processes point locations that
are passed to downstream functions (like
[`chelsa()`](https://animalbiodiversitylab.github.io/envar/reference/chelsa.md),
[`worldclim()`](https://animalbiodiversitylab.github.io/envar/reference/worldclim.md),
[`topography()`](https://animalbiodiversitylab.github.io/envar/reference/topography.md),
etc.) to ensure all retrieved variables are perfectly aligned and
stacked.

## How it works

1.  **Extent Definition:** You can define the study area using a
    shapefile (`sf` object), a `country` name, a `continent` name, or
    various biogeographic boundary types.

2.  **Resolution:** The `res` argument sets the target resolution as a
    multiplier of the base 30 arc-seconds (~1 km at equator).

3.  **Buffering:** An optional buffer can be applied to expand the study
    area or create a sampling radius around points. The buffer is
    **always specified in kilometers**, regardless of the target CRS.
    The function automatically converts to the appropriate units
    (degrees for geographic CRS, meters for projected CRS).

4.  **Output:**

    - If the input is a **polygon** (or country/continent), it returns a
      list containing a target `SpatRaster` grid and a vector mask.

    - If the input is **points** (without a buffer), it returns the
      point coordinates for extraction.

    - If the input is **points with a buffer**, it creates a polygon
      geometry around the points and returns a grid, allowing you to
      download raster data for the area surrounding your points.

## Resampling and reprojection

Downstream functions align every layer to the target grid defined here
using
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html)/[`terra::project()`](https://rspatial.github.io/terra/reference/project.html).
Continuous layers are resampled with bilinear interpolation, while
categorical (factor) layers automatically use nearest-neighbour to avoid
creating invalid class codes. You can force a specific method for all
layers with, e.g., `options(envar.resample_method = "near")` (accepted
values are any `terra` resampling method, or `"auto"` for the default
behaviour described above).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with a country
italy_grid <- var_get(country = "Italy")

# Download with a shapefile
processed_alps <- var_get(shape = "Alps") %>% 
esalandcover(vars=c("ice"))

# With a projected CRS and positive buffer (expand by 10 km)
italy_buffered <- var_get(country = "Italy", crs = 3035, buffer = 10)

# With a negative buffer (shrink by 10 km to exclude coastal areas)
italy_inland <- var_get(country = "Italy", crs = 3035, buffer = -10)

# Points with buffer to create extraction area
points_area <- var_get(pointsdf = Apollo, buffer = 10, crs = 4326)

# Using alpha hull to define species range from occurrence points
species_range <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE)

# Alpha hull with buffer (buffer applied after alpha hull creation)
species_range_buffered <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, buffer = 50)

# Alpha hull clipped to land boundary
species_range_land <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, land = TRUE)

# Alpha hull with buffer and land intersection
species_range_full <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, buffer = 25, land = TRUE)

# Using zoogeographic regions
palearctic <- var_get(zoorealm = "Palearctic")

# Using mountain regions
alps_gmba <- var_get(mountain_region = "European Alps")

# Using glacier regions
arctic_glaciers <- var_get(glacier_region_20 = "Arctic Canada North")

# Using freshwater ecoregions
danube <- var_get(freshwater_ecoregion = 404)

# Using marine ecoregions
mediterranean <- var_get(marine_realm = "Temperate Northern Atlantic")

# Using pelagic provinces
atlantic_pelagic <- var_get(pelagic_realm = "Atlantic")

# Clip marine realm to land only
land_only <- var_get(marine_realm = "Temperate Northern Atlantic", land = TRUE)
} # }
```
