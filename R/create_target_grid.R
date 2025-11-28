# R/create_target_grid.R
#' Create target grid for raster processing
#' @noRd
create_target_grid <- function(bbox, res = 1, crs = "EPSG:4326") {
  
  # Base resolution in degrees (30 arc-seconds)
  base_res <- 0.008333333
  target_res <- base_res * res
  
  # Get the CRS of the bbox
  bbox_crs <- sf::st_crs(bbox)
  target_crs <- sf::st_crs(crs)
  
  # Check if target CRS is geographic or projected
  is_target_geographic <- tryCatch({
    target_crs$IsGeographic
  }, error = function(e) {
    grepl("degree|longlat|latlong", target_crs$wkt, ignore.case = TRUE)
  })
  
  if (isTRUE(is_target_geographic)) {
    # For geographic CRS, use degrees resolution
    grid_res <- target_res
  } else {
    # For projected CRS, convert to meters (approximately)
    # 30 arc-seconds ≈ ~1000 meters at equator
    grid_res <- res * 1000
  }
  
  # Create grid directly in target CRS
  temp_grid <- terra::rast(
    xmin = bbox["xmin"],
    xmax = bbox["xmax"],
    ymin = bbox["ymin"],
    ymax = bbox["ymax"],
    resolution = grid_res,
    crs = crs
  )
  
  return(temp_grid)
}