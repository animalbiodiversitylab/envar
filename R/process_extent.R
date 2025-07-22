# R/process_extent.R
#' Process extent input
#' @noRd
process_extent <- function(extent, buffer_km) {
  extent_info <- list(type = NULL, bbox = NULL, mask = NULL, points = NULL)
  
  # Handle different input types
  if (inherits(extent, "sf") || inherits(extent, "sfc")) {
    # Shapefile or sf object
    extent_info$type <- "polygon"
    extent_info$mask <- extent
    if (buffer_km > 0) {
      extent_buffered <- sf::st_buffer(extent, dist = buffer_km * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      extent_info$bbox <- sf::st_bbox(extent)
    }
    
  } else if (is.character(extent)) {
    # Country or continent name
    extent_info$type <- "admin"
    
    # Try country first
    tryCatch({
      extent_info$mask <- rnaturalearth::ne_countries(
        country = extent, 
        returnclass = "sf",
        scale = "medium"
      )
    }, error = function(e) {
      # Try continent
      extent_info$mask <- rnaturalearth::ne_countries(
        continent = extent,
        returnclass = "sf",
        scale = "medium"
      )
    })
    
    if (is.null(extent_info$mask) || nrow(extent_info$mask) == 0) {
      cli::cli_abort("Could not find country or continent: {.val {extent}}")
    }
    
    if (buffer_km > 0) {
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_km * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
  } else if (is.matrix(extent) || is.data.frame(extent)) {
    # Points (for extraction)
    extent_info$type <- "points"
    extent_info$points <- sf::st_as_sf(
      as.data.frame(extent), 
      coords = c(1, 2), 
      crs = 4326
    )
    
    if (buffer_km > 0) {
      extent_buffered <- sf::st_buffer(extent_info$points, dist = buffer_km * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      # Add small buffer for point extraction
      extent_buffered <- sf::st_buffer(extent_info$points, dist = 10000) # 10km default
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    }
    
  } else {
    cli::cli_abort("Extent must be an sf object, country/continent name, or coordinate matrix")
  }
  
  return(extent_info)
}