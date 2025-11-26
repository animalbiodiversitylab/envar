# R/var_get.R

#' Initialize the Environmental Variable Retrieval Pipeline
#'
#' `var_get()` is the entry point for the **envar** package workflow. It defines the
#' spatial extent, resolution, and coordinate reference system (CRS) for the study area.
#'
#' This function does not download data itself. Instead, it creates a standardized
#' spatial template (grid) or processes point locations that are passed to downstream
#' functions (like `chelsa()`, `worldclim()`, `topography()`, etc.) to ensure all
#' retrieved variables are perfectly aligned and stacked.
#'
#' ## How it works
#' 1. **Extent Definition:** You can define the study area using a shapefile (`sf` object),
#'    a `country` name, or a `continent` name.
#' 2. **Resolution:** The `res` argument sets the target resolution in kilometers (approximate
#'    at the equator, based on arc-seconds).
#' 3. **Buffering:** An optional buffer can be applied to expand the study area or create
#'    a sampling radius around points.
#' 4. **Output:**
#'    - If the input is a **polygon** (or country/continent), it returns a list containing
#'      a target `SpatRaster` grid and a vector mask.
#'    - If the input is **points** (without a buffer), it returns the point coordinates for extraction.
#'    - If the input is **points with a buffer**, it creates a polygon geometry around the points
#'      and returns a grid, allowing you to download raster data for the area surrounding your points.
#'
#' @param shape An `sf` object representing the study area. This can be:
#'   * **Polygons:** defining a region of interest.
#'   * **Points:** defining specific sampling locations.
#'   If `shape` is provided, `country` and `continent` are ignored.
#'   If `shape` is provided, it must be already in the WGS84 projection.
#' @param country Character. The English name of a country (e.g., `"Italy"`, `"Viet Nam"`).
#'   Used to generate the extent if `shape` is NULL.
#' @param continent Character. The English name of a continent (e.g., `"Europe"`, `"Africa"`).
#'   Used to generate the extent if `shape` and `country` are NULL.
#' @param buffer Numeric. A buffer distance in **kilometers** to expand the extent.
#'   * For **polygons/countries**: The area is expanded outwards by this distance.
#'   * For **points**: A circular buffer of this radius is drawn around each point, effectively
#'     converting the study area into polygons (useful for downloading background raster data
#'     around occurrence points).
#'   * Default is `0`.
#' @param res Numeric. The target spatial resolution in **kilometers** (approximate).
#'   * This controls the cell size of the output raster stack.
#'   * Must be a positive integer (e.g., `1`, `5`, `10`).
#'   * Default is `1` (approx. 30 arc-seconds).
#'   * *Note:* Downstream functions will resample downloaded data to match this resolution.
#' @param path Character. Optional path to a local directory for saving intermediate files
#'   or outputs. (Currently reserved for future implementation).
#' @param crs Character or Numeric. The Coordinate Reference System for the **final output**.
#'   * Can be an EPSG code (e.g., `4326`, `3035`), a PROJ4 string, or WKT.
#'   * If `NULL` (default), the pipeline uses the standard WGS84 (EPSG:4326).
#'   * If specified, all downstream environmental layers will be projected to this CRS
#'     after processing.
#'
#' @return A `list` object (class `envar_par`) containing:
#'   * `grid`: A template `SpatRaster` defining the resolution and extent.
#'   * `mask`: An `sf` object defining the exact study area boundaries.
#'   * `res`: The resolution used.
#'   * `bbox`: The bounding box of the study area.
#'   * `crs`: The target coordinate reference system.
#'   * `type`: The type of input ("polygon" or "point").
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(envar)
#'
#' # ----------------------------------------------------------------
#' # Example 1: Working with a Country (Polygons)
#' # ----------------------------------------------------------------
#' # Initialize a workflow for Italy at 1km resolution
#' # and pipe it into a download function
#' data_stack <- var_get(country = "Italy", res = 1) %>%
#'   topography(vars = "elevation", topo_source = "GMTED")
#'
#' # ----------------------------------------------------------------
#' # Example 2: Working with Points (Extraction)
#' # ----------------------------------------------------------------
#' # Define some points (e.g., species occurrences)
#' my_points <- data.frame(lon = c(12.5, 13.0), lat = c(42.0, 42.5))
#' my_points_sf <- sf::st_as_sf(my_points, coords = c("lon", "lat"), crs = 4326)
#'
#' # Initialize extraction workflow (res is ignored for pure point extraction)
#' point_data <- var_get(shape = my_points_sf) %>%
#'   worldclim(vars = "bio")
#'
#' # ----------------------------------------------------------------
#' # Example 3: Working with Points + Buffer (Background Raster)
#' # ----------------------------------------------------------------
#' # Create a 50km buffer around points to get environmental rasters
#' # for the surrounding area (e.g., for pseudo-absence generation)
#' background_stack <- var_get(shape = my_points_sf, buffer = 50, res = 5) %>%
#'   chelsa(vars = "prec")
#'
#' # ----------------------------------------------------------------
#' # Example 4: Reprojection
#' # ----------------------------------------------------------------
#' # Get data for France, but project everything to Lambert-93 (EPSG:2154)
#' france_data <- var_get(country = "France", res = 1, crs = 2154) %>%
#'   accessibility(vars = "cities1")
#' }

var_get <- function(shape = NULL,
                    country = NULL,
                    continent = NULL,
                    buffer = 0,
                    res = NULL,
                    path = NULL,
                    crs = NULL) {   
  
  if (!is.null(country) | !is.null(continent)) {
    
    if (is.null(res)){
      res <- 1
    }
    
    if (!is.numeric(res) || res < 1 || res != as.integer(res)) {
      stop("Resolution not valid. Select a positive integer >= 1")
    }
    
    extent_info <- process_extent(
      shape = shape,
      country = country,
      continent = continent,
      buffer = buffer
    )
    
  } else {
    
    if (is.null(res)){
      res <- 1
    }
    
    if (!((sf::st_geometry_type(shape) == "POINT")[1] & buffer == 0)) {
      if (!is.numeric(res) || res < 1 || res != as.integer(res)) {
        stop("Resolution not valid. Select a positive integer > or equal to 1")
      }
    }
    
    extent_info <- process_extent(
      shape = shape,
      country = country,
      continent = continent,
      buffer = buffer
    )
  }
  
  
  # --------------------------------------------------------------------
  # attach CRS to extent_info for later use 
  # --------------------------------------------------------------------
  extent_info$crs <- crs 
  
  
  # --------------------------------------------------------------------
  # If non-point → return grid + mask + stored CRS
  # --------------------------------------------------------------------
  if (extent_info$type != "point") {
    target_grid <- create_target_grid(extent_info$bbox, res)
    
    return(list(
      grid = target_grid,
      mask = extent_info$mask,
      res = res,
      bbox = extent_info$bbox,
      crs = extent_info$crs                
    ))
  }
  
  # --------------------------------------------------------------------
  # If point → return extent_info including CRS
  # --------------------------------------------------------------------
  if (extent_info$type == "point") {
    return(extent_info)
  }
}
