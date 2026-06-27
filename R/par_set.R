# R/par_set.R

#' Initialize the Environmental Variable Retrieval Pipeline
#'
#' \code{par_set()} is the entry point for the \strong{envar} package workflow. It defines the
#' spatial extent, resolution, and coordinate reference system (CRS) for the study area.
#'
#' This function does not download data itself. Instead, it creates a standardized
#' spatial template (grid) or processes point locations that are passed to downstream
#' functions (like \code{chelsa()}, \code{worldclim()}, \code{topography()}, etc.) to ensure all
#' retrieved variables are perfectly aligned and stacked.
#'
#' @section How it works:
#' \enumerate{
#'   \item \strong{Extent Definition:} You can define the study area using a shapefile (\code{sf} object),
#'    a \code{country} name, a \code{continent} name, or various biogeographic boundary types.
#'   \item \strong{Resolution:} The \code{res} argument sets the target resolution as a multiplier of
#'    the base 30 arc-seconds (~1 km at equator).
#'   \item \strong{Buffering:} An optional buffer can be applied to expand the study area or create
#'    a sampling radius around points. The buffer is \strong{always specified in kilometers},
#'    regardless of the target CRS. The function automatically converts to the appropriate
#'    units (degrees for geographic CRS, meters for projected CRS).
#'   \item \strong{Output:}
#'    \itemize{
#'      \item If the input is a \strong{polygon} (or country/continent), it returns a list containing
#'      a target \code{SpatRaster} grid and a vector mask.
#'      \item If the input is \strong{points} (without a buffer), it returns the point coordinates for extraction.
#'      \item If the input is \strong{points with a buffer}, it creates a polygon geometry around the points
#'      and returns a grid, allowing you to download raster data for the area surrounding your points.
#'    }
#' }
#'
#' @param shape An \code{sf} object representing the study area. This can be:
#'   \itemize{
#'     \item \strong{Polygons:} defining a region of interest.
#'     \item \strong{Points:} defining specific sampling locations.
#'   }
#'   If \code{shape} is provided, \code{country} and \code{continent} are ignored.
#' @param country Character. The English name of a country (e.g., \code{"Italy"}, \code{"Viet Nam"}).
#'   Used to generate the extent if \code{shape} is \code{NULL}.
#' @param continent Character. The English name of a continent (e.g., \code{"Europe"}, \code{"Africa"}).
#'   Used to generate the extent if \code{shape} and \code{country} are \code{NULL}.
#' @param ecoregion Character. The name of a terrestrial ecoregion from Dinerstein et al. (2017).
#'   Uses the \code{ECO_NAME} column from the Ecoregions2017 dataset.
#' @param biome Character. The name of a biome from Dinerstein et al. (2017).
#'   Uses the \code{BIOME_NAME} column from the Ecoregions2017 dataset.
#' @param realm Character. The name of a biogeographic realm from Dinerstein et al. (2017).
#'   Uses the \code{REALM} column from the Ecoregions2017 dataset.
#' @param zooregion Character. The name of a zoogeographic region from Holt et al. (2013).
#'   Uses the \code{Regions} column from the CMEC dataset.
#' @param zoorealm Character. The name of a zoogeographic realm from Holt et al. (2013).
#'   Uses the \code{Realm} column from the CMEC newRealms dataset.
#' @param mountain_region Character. The name of a mountain region from the GMBA Mountain
#'   Inventory v2.0 (Snethlage et al. 2022). Uses the \code{MapName} column.
#' @param mountain_region_cmec Character. The name of a mountain region from the Center for Macroecology, 
#'   Evolution, and Climate definition of mountain areas (Rahbek et al. 2019). Uses the \code{Name} column.
#' @param glacier_region_19 Character. The name of a glacier region based on RGI v6.0 (2017)
#'   first-order regions. Uses the \code{RGI_CODE} column.
#' @param glacier_region_20 Character. The name of a glacier region based on GTN-G 2023
#'   first-order regions. Uses the \code{o1region} column.
#' @param freshwater_ecoregion Character or Numeric. The \code{FEOW_ID} of a freshwater ecoregion
#'   from Abell et al. (2008). Uses the \code{FEOW_ID} column from the FEOW dataset.
#' @param marine_ecoregion Character. The name of a marine ecoregion from Spalding et al. (2007).
#'   Uses the \code{ECOREGION} column from the MEOW dataset (\code{TYPE == "MEOW"}).
#' @param marine_realm Character. The name of a marine realm from Spalding et al. (2007).
#'   Uses the \code{REALM} column from the MEOW dataset (\code{TYPE == "MEOW"}).
#' @param marine_province Character. The name of a marine province from Spalding et al. (2007).
#'   Uses the \code{PROVINC} column from the MEOW dataset (\code{TYPE == "MEOW"}).
#' @param pelagic_province Character. The name of a pelagic province from Spalding et al. (2012).
#'   Uses the \code{PROVINC} column from the PPOW dataset (\code{TYPE == "PPOW"}).
#' @param pelagic_biome Character. The name of a pelagic biome from Spalding et al. (2012).
#'   Uses the \code{BIOME} column from the PPOW dataset (\code{TYPE == "PPOW"}).
#' @param pelagic_realm Character. The name of a pelagic realm from Spalding et al. (2012).
#'   Uses the \code{REALM} column from the PPOW dataset (\code{TYPE == "PPOW"}).
#' @param pointsdf Data.frame with columns \code{X} and \code{Y} representing point coordinates.
#' @param alpha_hull Logical. If \code{TRUE}, creates an alpha hull polygon around the occurrence
#'   points using the \code{getDynamicAlphaHull} function from the \pkg{rangeBuilder} package (Rabosky et al. 
#'   2016) to model species distribution ranges. The \code{initialAlpha} is set to 2 and gradually 
#'   increased until a polygon captures at least 99\% of occurrence records. This method optimizes 
#'   the balance between identifying distinct clusters as unique polygons and avoiding excessive 
#'   fragmentation (Roll et al. 2017). Can be used in conjunction with \code{buffer} (applied after 
#'   alpha hull creation) and \code{land} (intersects result with land boundary). Default is \code{FALSE}.
#'   
#'   References:
#'   \itemize{
#'     \item Rabosky ARD, et al. (2016). BAMMtools: an R package for the analysis of evolutionary 
#'     dynamics on phylogenetic trees. Methods in Ecology and Evolution 7:701-707.
#'     \item Roll U, et al. (2017). The global distribution of tetrapods reveals a need for targeted 
#'     reptile conservation. Nature Ecology & Evolution 1:1677-1682.
#'   }
#' @param buffer Numeric. A buffer distance in \strong{kilometers} to expand or shrink the extent.
#'   The buffer is always specified in kilometers regardless of the target CRS - the function
#'   automatically converts to the appropriate units internally (degrees for geographic CRS
#'   like EPSG:4326, meters for projected CRS like EPSG:3035 or ESRI:54009).
#'   \itemize{
#'     \item \strong{Positive values}: Expand the area outward by this distance.
#'     \item \strong{Negative values}: Shrink the area inward by this distance (useful for excluding
#'     coastal/border areas where data may have different characteristics).
#'     \item For \strong{points with positive buffer}: A circular buffer of this radius is drawn around 
#'     each point, effectively converting the study area into polygons.
#'     \item Default is \code{0}.
#'   }
#' @param res Numeric. The target spatial resolution multiplier.
#'   \itemize{
#'     \item This controls the cell size of the output raster stack.
#'     \item Must be a positive number \code{>= 1} (e.g., \code{1}, \code{5}, \code{10}).
#'       It is usually an integer, but fractional multipliers are allowed to match a
#'       dataset's native grid. For instance \code{biooracle()} requires \code{res = 6},
#'       which reproduces Bio-ORACLE's native 0.05° (~5.5 km) grid exactly.
#'     \item Default is \code{1} (30 arc-seconds or 0.008333333° at the equator).
#'     \item Higher values will multiply the original 30 arcsec resolution by the specified factor.
#'   }
#' @param path Character. Optional path to a local directory for saving intermediate files
#'   or outputs. 
#' @param crs Character or Numeric. The Coordinate Reference System for the \strong{final output}.
#'   \itemize{
#'     \item Can be an EPSG code with or without prefix (e.g., \code{4326}, \code{3035}, \code{"EPSG:4326"}), 
#'     an ESRI code (e.g., \code{54009}, \code{"ESRI:54009"}), a PROJ4 string, or WKT.
#'     \item If \code{NULL}, the pipeline uses the standard default WGS84 (\code{EPSG:4326}).
#'     \item If specified, all downstream environmental layers will be projected to this CRS
#'     after processing.
#'     \item Note: ESRI codes (53000-54999, 100000+) are automatically recognized and prefixed
#'     with "ESRI:" internally.
#'   }
#' @param scale Character with value \code{"small"}, \code{"medium"}, or \code{"large"}. It represents the scale at
#'   which the country/continent shapefile are retrieved using the \pkg{rnaturalearthdata} package. Large implies a better
#'   definition of the borders of the shapefile (scale 1:10). The default is \code{"medium"}. It is useful only when setting the argument \code{country} or \code{continent}.
#' @param set_na Logical, with default \code{FALSE}. If \code{TRUE}, any cell that is \code{NA} in at least one raster is set to be
#'   \code{NA} in all rasters of the final \code{SpatRaster} object. It is useful only when the output is a \code{SpatRaster} and not a point extraction.
#' @param land Logical, with default \code{FALSE}. If \code{TRUE}, the extent is intersected with the global land boundary
#'   from Natural Earth (at the scale defined by the \code{scale} argument). This is useful for clipping marine/pelagic 
#'   regions to land only, or for ensuring that buffered areas do not extend into the ocean. 
#'   Note: This does not apply to point extractions (\code{pointsdf} without buffer).
#' @param path directory to store the result of the download/processing. Default to \code{NULL} (no output is stored locally).
#'   It works only if no \code{corr_check()} is specified. Specify the path including the file name and the extension (e.g. \code{"../Out/rastername.tif"} if the final
#'   export is a \code{SpatRaster}; or \code{"../Out/extracteddataframe.csv"} if the output is a \code{data.frame}).
#' @param cache Logical, with default \code{TRUE}. If \code{TRUE}, each source file
#'   downloaded by the downstream functions (e.g. \code{chelsa()}, \code{worldclim()},
#'   \code{topography()}) is stored in a persistent per-user cache directory. If the
#'   download pipeline is interrupted (for example by a lost connection) and then
#'   re-launched, it resumes from where it stopped, reusing files that were already
#'   retrieved instead of downloading them again. Set to \code{FALSE} to use a
#'   temporary directory that is cleared at the end of the R session. The cache can
#'   be emptied at any time with \code{\link{clear_cache}}.
#'
#' @section Resampling and reprojection:
#' Downstream functions align every layer to the target grid defined here using
#' \code{terra::resample()}/\code{terra::project()}. Continuous layers are resampled
#' with bilinear interpolation, while categorical (factor) layers automatically use
#' nearest-neighbour to avoid creating invalid class codes. You can force a specific
#' method for all layers with, e.g., \code{options(envar.resample_method = "near")}
#' (accepted values are any \code{terra} resampling method, or \code{"auto"} for the
#' default behaviour described above).
#' @return A \code{list} object (class \code{envar_par}) containing:
#'   \itemize{
#'     \item \code{grid}: A template \code{SpatRaster} defining the resolution and extent (for polygon input).
#'     \item \code{mask}: An \code{sf} object defining the exact study area boundaries (for polygon input).
#'     \item \code{res}: The resolution multiplier used.
#'     \item \code{bbox}: The bounding box of the study area.
#'     \item \code{crs}: The target coordinate reference system.
#'     \item \code{type}: The type of input (\code{"polygon"}, \code{"admin"}, or \code{"point"}).
#'     \item \code{is_global}: Logical, \code{TRUE} if processing global extent.
#'     \item \code{set_na}: Logical, \code{TRUE} if user wants to apply an NA mask.
#'     \item \code{path}: User-specified path to store the result.
#'   }
#' @examples
#' \dontrun{
#' # Basic usage with a country
#' italy_grid <- par_set(country = "Italy")
#' 
#' # Download with a shapefile
#' processed_alps <- par_set(shape = "Alps") %>% 
#' melc(vars=c("ice"))
#' 
#' # With a projected CRS and positive buffer (expand by 10 km)
#' italy_buffered <- par_set(country = "Italy", crs = 3035, buffer = 10)
#' 
#' # With a negative buffer (shrink by 10 km to exclude coastal areas)
#' italy_inland <- par_set(country = "Italy", crs = 3035, buffer = -10)
#' 
#' # Points with buffer to create extraction area
#' points_area <- par_set(pointsdf = Apollo, buffer = 10, crs = 4326)
#' 
#' # Using alpha hull to define species range from occurrence points
#' species_range <- par_set(pointsdf = species_occurrences, alpha_hull = TRUE)
#' 
#' # Alpha hull with buffer (buffer applied after alpha hull creation)
#' species_range_buffered <- par_set(pointsdf = species_occurrences, alpha_hull = TRUE, buffer = 50)
#' 
#' # Alpha hull clipped to land boundary
#' species_range_land <- par_set(pointsdf = species_occurrences, alpha_hull = TRUE, land = TRUE)
#' 
#' # Alpha hull with buffer and land intersection
#' species_range_full <- par_set(pointsdf = species_occurrences, alpha_hull = TRUE,
#'                                buffer = 25, land = TRUE)
#' 
#' # Using zoogeographic regions
#' palearctic <- par_set(zoorealm = "Palearctic")
#' 
#' # Using mountain regions
#' alps_gmba <- par_set(mountain_region = "European Alps")
#' 
#' # Using glacier regions
#' arctic_glaciers <- par_set(glacier_region_20 = "Arctic Canada North")
#' 
#' # Using freshwater ecoregions
#' danube <- par_set(freshwater_ecoregion = 404)
#' 
#' # Using marine ecoregions
#' mediterranean <- par_set(marine_realm = "Temperate Northern Atlantic")
#' 
#' # Using pelagic provinces
#' atlantic_pelagic <- par_set(pelagic_realm = "Atlantic")
#' 
#' # Clip marine realm to land only
#' land_only <- par_set(marine_realm = "Temperate Northern Atlantic", land = TRUE)
#' }
#'
#' @export

par_set <- function(country = NULL,
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
                    cache = TRUE) {

  if (is.null(res)) {
    res <- 1
  }

  # Validate and activate the download cache (see envar_grids_dir()).
  if (!is.logical(cache) || length(cache) != 1 || is.na(cache)) {
    cli::cli_abort("{.arg cache} must be a single logical value (TRUE or FALSE).")
  }
  options(envar.cache = cache)
  if (isTRUE(cache)) {
    cli::cli_alert_info(
      "Download cache is ON: processed source files are stored and reused on re-runs. Set {.code cache = FALSE} to disable, or call {.fn clear_cache} to empty it."
    )
  }
  
  if (!is.numeric(res) || length(res) != 1 || is.na(res) || res < 1) {
    cli::cli_abort(c(
      "{.arg res} must be a single number greater than or equal to 1.",
      "x" = "You supplied {.val {res}}.",
      "i" = "{.arg res} multiplies the ~1 km (30 arc-second) base grid: 1 keeps the native resolution, higher values aggregate to coarser cells."
    ))
  }

  # Validate scale (used when retrieving country/continent boundaries)
  if (!is.character(scale) || length(scale) != 1 || !scale %in% c("small", "medium", "large")) {
    cli::cli_abort(c(
      "{.arg scale} must be one of {.val small}, {.val medium}, or {.val large}.",
      "x" = "You supplied {.val {scale}}."
    ))
  }

  # Validate buffer
  if (!is.numeric(buffer) || length(buffer) != 1 || is.na(buffer)) {
    cli::cli_abort("{.arg buffer} must be a single numeric value, in kilometres.")
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

  # Validate that the (normalized) CRS can actually be understood by sf/PROJ
  crs_obj <- tryCatch(suppressWarnings(sf::st_crs(crs)), error = function(e) NULL)
  if (is.null(crs_obj) || isTRUE(is.na(crs_obj))) {
    cli::cli_abort(c(
      "{.arg crs} {.val {crs}} is not a valid coordinate reference system.",
      "i" = "Use an EPSG or ESRI code (e.g. {.val 4326}, {.val 3035}, {.val 54009}), or a PROJ4/WKT string."
    ))
  }

  # Check for global extent with buffer
  is_global <- is.null(country) && is.null(continent) && is.null(shape) && 
    is.null(pointsdf) && is.null(realm) && is.null(ecoregion) && is.null(biome) &&
    is.null(zooregion) && is.null(zoorealm) && is.null(mountain_region) && is.null(mountain_region_cmec) &&
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
      cli::cli_abort(c(
        "{.arg pointsdf} must be a {.cls data.frame} with columns {.field X} and {.field Y}, or an {.cls sf} points object.",
        "i" = "To use a polygon study area, pass it to {.arg shape} instead of {.arg pointsdf}."
      ))
    }

    if (inherits(pointsdf, "sf")) {
      # pointsdf is for points only; a polygon/line sf belongs in `shape`
      geom_types <- unique(as.character(sf::st_geometry_type(pointsdf)))
      if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) {
        cli::cli_abort(c(
          "{.arg pointsdf} must contain point geometries.",
          "x" = "The {.cls sf} object you supplied contains {.val {geom_types}} geometr{?y/ies}.",
          "i" = "For polygons or lines, pass the object to {.arg shape} instead of {.arg pointsdf}."
        ))
      }
      # If it's already sf, just use it as shape
      shape <- pointsdf
      cli::cli_alert_info("Using sf object from pointsdf with CRS: {sf::st_crs(shape)$input}")
    } else if (is.data.frame(pointsdf)) {
      if (!all(c("X", "Y") %in% names(pointsdf))) {
        cli::cli_abort(c(
          "{.arg pointsdf} must be a {.cls data.frame} with columns {.field X} and {.field Y}.",
          "x" = "Found columns: {.field {names(pointsdf)}}."
        ))
      }
      # Footgun guard: if the coordinates look like longitude/latitude (degrees)
      # but the target CRS is projected, st_as_sf() *labels* them as that CRS
      # without reprojecting, silently mislocating the points. Warn only; the
      # behaviour is unchanged.
      is_geographic <- tryCatch(isTRUE(sf::st_crs(crs)$IsGeographic),
                                error = function(e) NA)
      looks_lonlat <- all(abs(pointsdf$X) <= 180, na.rm = TRUE) &&
                      all(abs(pointsdf$Y) <= 90, na.rm = TRUE)
      if (isFALSE(is_geographic) && isTRUE(looks_lonlat)) {
        cli::cli_warn(c(
          "{.arg pointsdf} coordinates look like longitude/latitude (degrees), but {.arg crs} ({.val {crs}}) is a projected CRS.",
          "!" = "They will be labelled as {.val {crs}} without reprojection, which can mislocate the points.",
          "i" = "If the coordinates are WGS84, use {.code crs = 4326}."
        ))
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
    mountain_region_cmec = mountain_region_cmec,
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
  extent_info$cache <- cache
  
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
      from_parset = TRUE,
      set_na = set_na,
      path = path,
      land=land,
      cache = cache
    )
    class(result) <- c("envar_par", "list")
    return(result)
  }
  
  # If point -> return extent_info including CRS
  if (extent_info$type == "point") {
    extent_info$res <- res
    extent_info$is_global <- FALSE
    extent_info$from_parset <- TRUE
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
  
  # Use a fixed (non-scientific) representation so large numeric codes such as
  # 100000 are not turned into "1e+05" by as.character().
  crs_str <- if (is.numeric(crs)) format(crs, scientific = FALSE, trim = TRUE) else as.character(crs)
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