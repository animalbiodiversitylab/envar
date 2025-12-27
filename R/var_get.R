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
#'    a `country` name, a `continent` name, or various biogeographic boundary types.
#' 2. **Resolution:** The `res` argument sets the target resolution as a multiplier of
#'    the base 30 arc-seconds (~1 km at equator).
#' 3. **Buffering:** An optional buffer can be applied to expand the study area or create
#'    a sampling radius around points. The buffer is **always specified in kilometers**,
#'    regardless of the target CRS. The function automatically converts to the appropriate
#'    units (degrees for geographic CRS, meters for projected CRS).
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
#' @param country Character. The English name of a country (e.g., `"Italy"`, `"Viet Nam"`).
#'   Used to generate the extent if `shape` is NULL.
#' @param continent Character. The English name of a continent (e.g., `"Europe"`, `"Africa"`).
#'   Used to generate the extent if `shape` and `country` are NULL.
#' @param ecoregion Character. The name of a terrestrial ecoregion from Dinerstein et al. (2017).
#'   Uses the ECO_NAME column from the Ecoregions2017 dataset.
#' @param biome Character. The name of a biome from Dinerstein et al. (2017).
#'   Uses the BIOME_NAME column from the Ecoregions2017 dataset.
#' @param realm Character. The name of a biogeographic realm from Dinerstein et al. (2017).
#'   Uses the REALM column from the Ecoregions2017 dataset.
#' @param zooregion Character. The name of a zoogeographic region from Holt et al. (2013).
#'   Uses the Regions column from the CMEC dataset.
#' @param zoorealm Character. The name of a zoogeographic realm from Holt et al. (2013).
#'   Uses the Realm column from the CMEC newRealms dataset.
#' @param mountain_region Character. The name of a mountain region from the GMBA Mountain
#'   Inventory v2.0 (Snethlage et al. 2022). Uses the MapName column.
#' @param glacier_region_19 Character. The name of a glacier region based on RGI v6.0 (2017)
#'   first-order regions. Uses the RGI_CODE column.
#' @param glacier_region_20 Character. The name of a glacier region based on GTN-G 2023
#'   first-order regions. Uses the o1region column.
#' @param freshwater_ecoregion Character or Numeric. The FEOW_ID of a freshwater ecoregion
#'   from Abell et al. (2008). Uses the FEOW_ID column from the FEOW dataset.
#' @param marine_ecoregion Character. The name of a marine ecoregion from Spalding et al. (2007).
#'   Uses the ECOREGION column from the MEOW dataset (TYPE == "MEOW").
#' @param marine_realm Character. The name of a marine realm from Spalding et al. (2007).
#'   Uses the REALM column from the MEOW dataset (TYPE == "MEOW").
#' @param marine_province Character. The name of a marine province from Spalding et al. (2007).
#'   Uses the PROVINC column from the MEOW dataset (TYPE == "MEOW").
#' @param pelagic_province Character. The name of a pelagic province from Spalding et al. (2012).
#'   Uses the PROVINC column from the PPOW dataset (TYPE == "PPOW").
#' @param pelagic_biome Character. The name of a pelagic biome from Spalding et al. (2012).
#'   Uses the BIOME column from the PPOW dataset (TYPE == "PPOW").
#' @param pelagic_realm Character. The name of a pelagic realm from Spalding et al. (2012).
#'   Uses the REALM column from the PPOW dataset (TYPE == "PPOW").
#' @param pointsdf Data.frame with columns X and Y representing point coordinates.
#' @param alpha_hull Logical. If TRUE, creates an alpha hull polygon around the occurrence
#'   points using the `getDynamicAlphaHull` function from the rangeBuilder package (Rabosky et al. 
#'   2016) to model species distribution ranges. The initialAlpha is set to 2 and gradually 
#'   increased until a polygon captures at least 99% of occurrence records. This method optimizes 
#'   the balance between identifying distinct clusters as unique polygons and avoiding excessive 
#'   fragmentation (Roll et al. 2017). Can be used in conjunction with `buffer` (applied after 
#'   alpha hull creation) and `land` (intersects result with land boundary). Default is FALSE.
#'   
#'   References:
#'   * Rabosky ARD, et al. (2016). BAMMtools: an R package for the analysis of evolutionary 
#'     dynamics on phylogenetic trees. Methods in Ecology and Evolution 7:701-707.
#'   * Roll U, et al. (2017). The global distribution of tetrapods reveals a need for targeted 
#'     reptile conservation. Nature Ecology & Evolution 1:1677-1682.
#' @param buffer Numeric. A buffer distance in **kilometers** to expand or shrink the extent.
#'   The buffer is always specified in kilometers regardless of the target CRS - the function
#'   automatically converts to the appropriate units internally (degrees for geographic CRS
#'   like EPSG:4326, meters for projected CRS like EPSG:3035 or ESRI:54009).
#'   * **Positive values**: Expand the area outward by this distance.
#'   * **Negative values**: Shrink the area inward by this distance (useful for excluding
#'     coastal/border areas where data may have different characteristics).
#'   * For **points with positive buffer**: A circular buffer of this radius is drawn around 
#'     each point, effectively converting the study area into polygons.
#'   * Default is `0`.
#' @param res Numeric. The target spatial resolution multiplier.
#'   * This controls the cell size of the output raster stack.
#'   * Must be a positive integer (e.g., `1`, `5`, `10`).
#'   * Default is `1` (30 arc-seconds or 0.008333333Â° at the equator).
#'   * Higher values will multiply the original 30 arcsec resolution by the specified factor.
#' @param path Character. Optional path to a local directory for saving intermediate files
#'   or outputs. 
#' @param crs Character or Numeric. The Coordinate Reference System for the **final output**.
#'   * Can be an EPSG code with or without prefix (e.g., `4326`, `3035`, `EPSG:4326`), 
#'     an ESRI code (e.g., `54009`, `ESRI:54009`), a PROJ4 string, or WKT.
#'   * If `NULL`, the pipeline uses the standard default WGS84 (EPSG:4326).
#'   * If specified, all downstream environmental layers will be projected to this CRS
#'     after processing.
#'   * Note: ESRI codes (53000-54999, 100000+) are automatically recognized and prefixed
#'     with "ESRI:" internally.
#' @param scale Character with value "small", "medium", or "large". It represents the scale at
#'   which the country/continent shapefile are retrieved using the rnaturalearthdata package. Large implies a better
#'   definition of the borders of the shapefile (scale 1:10). The default is "medium". It is useful only when setting the argument country or continent.
#' @param set_na Logical, with default FALSE. If TRUE, any cell that is NA in at least one raster is set to be
#'   NA in all rasters of the final SpatRaster object. It is useful only when the output is a SpatRaster and not a point extraction.
#' @param land Logical, with default FALSE. If TRUE, the extent is intersected with the global land boundary
#'   from Natural Earth (at the scale defined by the `scale` argument). This is useful for clipping marine/pelagic 
#'   regions to land only, or for ensuring that buffered areas do not extend into the ocean. 
#'   Note: This does not apply to point extractions (pointsdf without buffer).
#' @param path directory to store the result of the download/processing. Default to NULL (no output is stored locally).
#'   It works only if no corr_check() is specified. Specify the path including the file name and the extension (e.g. "../Out/rastername.tif" if the final
#'   export is a SpatRaster; or "../Out/extracteddataframe.csv" if the output is a data.frame).
#' @return A `list` object (class `envar_par`) containing:
#'   * `grid`: A template `SpatRaster` defining the resolution and extent (for polygon input).
#'   * `mask`: An `sf` object defining the exact study area boundaries (for polygon input).
#'   * `res`: The resolution multiplier used.
#'   * `bbox`: The bounding box of the study area.
#'   * `crs`: The target coordinate reference system.
#'   * `type`: The type of input ("polygon", "admin", or "point").
#'   * `is_global`: Logical, TRUE if processing global extent.
#'   * `set_na`: Logical, TRUE if user wants to apply an NA mask.
#'   * `path`: User-specified path to store the result.
#' @examples
#' \dontrun{
#' # Basic usage with a country
#' italy_grid <- var_get(country = "Italy")
#' 
#' # Download with a shapefile
#' processed_alps <- var_get(shape = "Alps") %>% 
#' esalandcover(vars=c("ice"))
#' 
#' # With a projected CRS and positive buffer (expand by 10 km)
#' italy_buffered <- var_get(country = "Italy", crs = 3035, buffer = 10)
#' 
#' # With a negative buffer (shrink by 10 km to exclude coastal areas)
#' italy_inland <- var_get(country = "Italy", crs = 3035, buffer = -10)
#' 
#' # Points with buffer to create extraction area
#' points_area <- var_get(pointsdf = Apollo, buffer = 10, crs = 4326)
#' 
#' # Using alpha hull to define species range from occurrence points
#' species_range <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE)
#' 
#' # Alpha hull with buffer (buffer applied after alpha hull creation)
#' species_range_buffered <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, buffer = 50)
#' 
#' # Alpha hull clipped to land boundary
#' species_range_land <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, land = TRUE)
#' 
#' # Alpha hull with buffer and land intersection
#' species_range_full <- var_get(pointsdf = species_occurrences, alpha_hull = TRUE, buffer = 25, land = TRUE)
#' 
#' # Using zoogeographic regions
#' palearctic <- var_get(zoorealm = "Palearctic")
#' 
#' # Using mountain regions
#' alps_gmba <- var_get(mountain_region = "European Alps")
#' 
#' # Using glacier regions
#' arctic_glaciers <- var_get(glacier_region_20 = "Arctic Canada North")
#' 
#' # Using freshwater ecoregions
#' danube <- var_get(freshwater_ecoregion = 404)
#' 
#' # Using marine ecoregions
#' mediterranean <- var_get(marine_realm = "Temperate Northern Atlantic")
#' 
#' # Using pelagic provinces
#' atlantic_pelagic <- var_get(pelagic_realm = "Atlantic")
#' 
#' # Clip marine realm to land only
#' land_only <- var_get(marine_realm = "Temperate Northern Atlantic", land = TRUE)
#' }
#'
#' @export
var_get <- function(country = NULL,
                    continent = NULL,
                    shape = NULL,
                    ecoregion = NULL,
                    biome = NULL,
                    realm = NULL,
                    zooregion = NULL,
                    zoorealm = NULL,
                    mountain_region = NULL,
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
                    land = FALSE) {
  
  if (is.null(res)) {
    res <- 1
  }
  
  if (!is.numeric(res) || res < 1 || res != as.integer(res)) {
    stop("Resolution not valid. Select a positive integer >= 1")
  }
  
  # Validate buffer
  if (!is.numeric(buffer) || length(buffer) != 1) {
    cli::cli_abort("Buffer must be a single numeric value in kilometers.")
  }
  
  # Validate alpha_hull
  if (!is.logical(alpha_hull) || length(alpha_hull) != 1) {
    cli::cli_abort("alpha_hull must be a single logical value (TRUE or FALSE).")
  }
  
  # alpha_hull requires pointsdf
  if (alpha_hull && is.null(pointsdf)) {
    cli::cli_abort("alpha_hull = TRUE requires pointsdf to be specified.")
  }
  
  # Normalize CRS to standard format (handles numeric codes for EPSG vs ESRI)
  crs <- normalize_crs(crs)
  
  # Check for global extent with buffer
  is_global <- is.null(country) && is.null(continent) && is.null(shape) && 
    is.null(pointsdf) && is.null(realm) && is.null(ecoregion) && is.null(biome) &&
    is.null(zooregion) && is.null(zoorealm) && is.null(mountain_region) &&
    is.null(glacier_region_19) && is.null(glacier_region_20) && 
    is.null(freshwater_ecoregion) && is.null(marine_ecoregion) && 
    is.null(marine_realm) && is.null(marine_province) &&
    is.null(pelagic_province) && is.null(pelagic_biome) && is.null(pelagic_realm)
  
  if (is_global && buffer != 0) {
    cli::cli_abort("Buffer cannot be applied when working at global scale (no shape/country/continent defined).")
  }
  
  # Handle pointsdf conversion to sf
  if (!is.null(pointsdf)) {
    if (!is.data.frame(pointsdf) && !inherits(pointsdf, "sf")) {
      cli::cli_abort("pointsdf must be a data.frame with columns 'X' and 'Y', or an sf object.")
    }
    
    if (inherits(pointsdf, "sf")) {
      # If it's already sf, just use it as shape
      shape <- pointsdf
      cli::cli_alert_info("Using sf object from pointsdf with CRS: {sf::st_crs(shape)$input}")
    } else if (is.data.frame(pointsdf)) {
      if (!all(c("X", "Y") %in% names(pointsdf))) {
        cli::cli_abort("pointsdf must be a data.frame with columns 'X' and 'Y'.")
      }
      # Note: We assume pointsdf input is usually WGS84 or matches the desired CRS if not specified
      shape <- sf::st_as_sf(pointsdf, coords = c("X", "Y"), crs = crs)
      cli::cli_alert_info("Converted pointsdf to sf object with CRS: {crs}")
    }
  }
  
  # Process extent
  extent_info <- process_extent(
    shape = shape,
    country = country,
    continent = continent,
    realm = realm,
    ecoregion = ecoregion,
    biome = biome,
    zooregion = zooregion,
    zoorealm = zoorealm,
    mountain_region = mountain_region,
    glacier_region_19 = glacier_region_19,
    glacier_region_20 = glacier_region_20,
    freshwater_ecoregion = freshwater_ecoregion,
    marine_ecoregion = marine_ecoregion,
    marine_realm = marine_realm,
    marine_province = marine_province,
    pelagic_province = pelagic_province,
    pelagic_biome = pelagic_biome,
    pelagic_realm = pelagic_realm,
    alpha_hull = alpha_hull,
    buffer = buffer,
    crs = crs,
    scale = scale,
    land = land
  )
  
  # Attach CRS and resolution to extent_info
  extent_info$crs <- crs
  extent_info$res <- res
  extent_info$is_global <- is_global
  extent_info$set_na <- set_na
  extent_info$path <- path
  extent_info$land <- land
  
  # If non-point -> return grid + mask + stored CRS
  if (extent_info$type != "point") {
    target_grid <- create_target_grid(extent_info$bbox, res = res, crs = crs)
    
    result <- list(
      grid = target_grid,
      mask = extent_info$mask,
      res = res,
      bbox = extent_info$bbox,
      crs = crs,
      type = extent_info$type,
      is_global = is_global,
      from_varget = TRUE,
      set_na = set_na,
      path = path,
      land=land
    )
    class(result) <- c("envar_par", "list")
    return(result)
  }
  
  # If point -> return extent_info including CRS
  if (extent_info$type == "point") {
    extent_info$res <- res
    extent_info$is_global <- FALSE
    extent_info$from_varget <- TRUE
    class(extent_info) <- c("envar_par", "list")
    return(extent_info)
  }
}




#' Normalize CRS to standard format
#' @noRd
normalize_crs <- function(crs) {
  if (is.null(crs)) {
    return("EPSG:4326")
  }
  
  crs_str <- as.character(crs)
  crs_str <- trimws(crs_str)
  
  # 1. If it already has EPSG: or ESRI: prefix, return with standardized casing
  if (grepl("^(EPSG|ESRI):", crs_str, ignore.case = TRUE)) {
    parts <- strsplit(crs_str, ":")[[1]]
    return(paste0(toupper(parts[1]), ":", parts[2]))
  }
  
  # 2. If it is just a number
  if (grepl("^[0-9]+$", crs_str)) {
    code_num <- as.numeric(crs_str)
    
    # Define ranges commonly reserved for ESRI authorities
    # 53000 - 53099: Sphere-based projections
    # 54000 - 54099: World projections (e.g., Mollweide, Robinson)
    # 102000 - 104xxx: ESRI Custom/Legacy
    is_esri <- (code_num >= 53000 & code_num <= 54999) | 
      (code_num >= 100000)
    
    if (is_esri) {
      return(paste0("ESRI:", crs_str))
    } else {
      return(paste0("EPSG:", crs_str))
    }
  }
  
  # 3. Otherwise return as-is (could be PROJ4 or WKT)
  return(crs_str)
}