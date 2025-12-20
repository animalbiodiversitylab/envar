# R/process_extent.R
#' Process extent input
#' @noRd
process_extent <- function(shape = NULL, country = NULL, continent = NULL, buffer = 0, crs = "EPSG:4326", scale="medium") {
  
  extent_info <- list(type = NULL, bbox = NULL, mask = NULL)
  
  # ---- Input validation & priority resolution ----
  input_sources <- list(
    shape = !is.null(shape),
    country = !is.null(country),
    continent = !is.null(continent)
  )
  
  
  active_sources <- names(Filter(identity, input_sources))
  
  if (length(active_sources) > 1) {
    cli::cli_alert_warning("You specified multiple extent sources: {paste(active_sources, collapse = ', ')}. The following priority will be used: shape > country > continent.")
  }
  
  # ---- 0. NOTHING SPECIFIED (Global extent) -----
  if (length(active_sources) == 0) {
    extent_info$type <- "polygon"
    extent_info$is_global <- TRUE
    
    # Default global extent in WGS84
    global_bbox <- sf::st_bbox(c(
      xmin = -180.00013888885,
      ymin = -90.00013888885,
      xmax = 179.99985967115,
      ymax = 83.99986041515
    ), crs = sf::st_crs(4326))
    
    extent_info$mask <- sf::st_sf(sf::st_as_sfc(global_bbox))
    
    # Transform to target CRS if different from WGS84
    target_crs <- sf::st_crs(crs)
    if (!is.na(target_crs) && !identical(sf::st_crs(4326), target_crs)) {
      extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    }
    
    extent_info$bbox <- sf::st_bbox(extent_info$mask)
    return(extent_info)
  }
  
  extent_info$is_global <- FALSE
  
  # ---- 1. SHAPE (highest priority) ----
  if (!is.null(shape)) {
    if (!inherits(shape, "sf") && !inherits(shape, "sfc")) {
      cli::cli_abort("The `shape` parameter must be an object of type `sf` or `sfc`.")
    }
    
    # Convert sfc to sf if needed
    if (inherits(shape, "sfc")) {
      shape <- sf::st_sf(geometry = shape)
    }
    
    # Ensure shape is in the target CRS
    shape_crs <- sf::st_crs(shape)
    target_crs_obj <- sf::st_crs(crs)
    
    if (!is.na(shape_crs) && !is.na(target_crs_obj) && !identical(shape_crs, target_crs_obj)) {
      shape <- sf::st_transform(shape, crs)
      cli::cli_alert_info("Transformed shape to target CRS: {crs}")
    } else if (is.na(shape_crs)) {
      # If shape has no CRS, assign the target CRS
      sf::st_crs(shape) <- crs
      cli::cli_alert_info("Assigned CRS {crs} to shape (was NA)")
    }
    
    # Check geometry type
    geom_types <- unique(as.character(sf::st_geometry_type(shape)))
    is_point <- all(geom_types %in% c("POINT", "MULTIPOINT"))
    
    if (is_point) {
      if (buffer != 0) {
        extent_info$type <- "polygon"
        
        # Convert buffer from km to appropriate units
        buffer_dist <- convert_buffer_to_units(buffer, crs)
        extent_buffered <- sf::st_buffer(shape, dist = buffer_dist)
        
        # Union all buffered points into single geometry
        extent_buffered <- sf::st_union(extent_buffered)
        extent_buffered <- sf::st_sf(geometry = extent_buffered)
        sf::st_crs(extent_buffered) <- crs
        
        extent_info$bbox <- sf::st_bbox(extent_buffered)
        extent_info$mask <- extent_buffered
      } else {
        extent_info$type <- "point"
        extent_info$bbox <- sf::st_bbox(shape)
        extent_info$mask <- shape
      }
    } else {
      extent_info$type <- "polygon"
      extent_info$mask <- shape
      
      if (buffer != 0) {
        buffer_dist <- convert_buffer_to_units(buffer, crs)
        extent_buffered <- sf::st_buffer(shape, dist = buffer_dist)
        extent_info$bbox <- sf::st_bbox(extent_buffered)
        extent_info$mask <- extent_buffered
      } else {
        extent_info$bbox <- sf::st_bbox(shape)
      }
    }
    
    return(extent_info)
  }
  
  # ---- 2. COUNTRY (second priority) ----
  if (!is.null(country)) {
    extent_info$type <- "admin"
    
    tryCatch({
      extent_info$mask <- rnaturalearth::ne_countries(
        country = country,
        returnclass = "sf",
        scale = scale
      )
    }, error = function(e) {
      cli::cli_abort("Country not found: {.val {country}}.")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ---- 3. CONTINENT (lowest priority) ----
  if (!is.null(continent)) {
    extent_info$type <- "admin"
    
    tryCatch({
      
      if (continent == "Europe" | continent == "europe"){
        
        extent_info$mask <- Europe
        
      } else {
        
      extent_info$mask <- rnaturalearth::ne_countries(
        continent = continent,
        returnclass = "sf",
        scale = scale
      )
      }
      
      
      
      
    }, error = function(e) {
      cli::cli_abort("Continent not found: {.val {continent}}.")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
}

#' Convert buffer from kilometers to appropriate units based on CRS
#' 
#' This function ensures a unified interface where users always specify buffers
#' in kilometers, and the function internally converts to the appropriate units
#' based on the coordinate reference system (degrees for geographic CRS, 
#' meters for projected CRS).
#' 
#' @param buffer_km Numeric. Buffer distance in kilometers (can be negative for inward buffers).
#' @param crs Character or numeric. The coordinate reference system.
#' @return Numeric. Buffer distance in the appropriate units for the CRS.
#' @noRd
convert_buffer_to_units <- function(buffer_km, crs) {
  crs_obj <- sf::st_crs(crs)
  
  if (is.na(crs_obj)) {
    # Default to geographic assumption if CRS is NA
    cli::cli_alert_warning("CRS is NA, assuming geographic coordinates for buffer conversion.")
    buffer_dist <- buffer_km / 111
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {round(buffer_dist, 6)} degrees (geographic CRS assumed)")
    return(buffer_dist)
  }
  
  # Determine if CRS is geographic (lat/lon) or projected (typically meters)
  is_geographic <- get_crs_type(crs_obj)
  
  if (isTRUE(is_geographic)) {
    # For geographic CRS, convert km to degrees (approximate)
    # 1 degree ≈ 111 km at equator
    buffer_dist <- buffer_km / 111
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {round(buffer_dist, 6)} degrees (geographic CRS)")
  } else {
    # For projected CRS, convert km to meters
    buffer_dist <- buffer_km * 1000
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {buffer_dist} meters (projected CRS)")
  }
  
  return(buffer_dist)
}

#' Determine if a CRS is geographic or projected
#' 
#' @param crs_obj An sf CRS object
#' @return Logical. TRUE if geographic, FALSE if projected.
#' @noRd
get_crs_type <- function(crs_obj) {
  # Try the direct IsGeographic property first (most reliable)
  is_geographic <- tryCatch({
    crs_obj$IsGeographic
  }, error = function(e) NULL)
  
  if (!is.null(is_geographic)) {
    return(is_geographic)
  }
  
  # Fallback: check the units in the WKT
  wkt <- crs_obj$wkt
  if (!is.null(wkt) && nchar(wkt) > 0) {
    # Check for degree units (geographic)
    if (grepl("UNIT\\[\"degree\"", wkt, ignore.case = TRUE) ||
        grepl("ANGLEUNIT\\[\"degree\"", wkt, ignore.case = TRUE)) {
      return(TRUE)
    }
    # Check for meter units (projected)
    if (grepl("UNIT\\[\"metre\"", wkt, ignore.case = TRUE) ||
        grepl("UNIT\\[\"meter\"", wkt, ignore.case = TRUE) ||
        grepl("LENGTHUNIT\\[\"metre\"", wkt, ignore.case = TRUE)) {
      return(FALSE)
    }
  }
  
  # Fallback: check proj4string patterns
  proj4 <- crs_obj$proj4string
  if (!is.null(proj4)) {
    if (grepl("\\+proj=longlat|\\+proj=latlong", proj4, ignore.case = TRUE)) {
      return(TRUE)
    }
    if (grepl("\\+units=m", proj4, ignore.case = TRUE)) {
      return(FALSE)
    }
  }
  
  # Last resort: check common patterns in the input
  input <- crs_obj$input
  if (!is.null(input)) {
    # EPSG:4326 and similar are geographic
    if (grepl("4326|4269|4267", input)) {
      return(TRUE)
    }
  }
  
  # Default assumption: if we can't determine, assume projected (meters)
  cli::cli_alert_warning("Could not determine CRS type, assuming projected (meters).")
  return(FALSE)
}