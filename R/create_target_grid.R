# R/create_target_grid.R
#' Create target grid at 30 arc-seconds
#' @noRd
create_target_grid <- function(bbox, resolution, crs) {
  # Always create at 30 arc-seconds (0.008333333 degrees)
  res_degrees <- 0.008333333
  
  # Expand bbox to align with grid
  xmin <- floor(bbox["xmin"] / res_degrees) * res_degrees
  xmax <- ceiling(bbox["xmax"] / res_degrees) * res_degrees
  ymin <- floor(bbox["ymin"] / res_degrees) * res_degrees
  ymax <- ceiling(bbox["ymax"] / res_degrees) * res_degrees
  
  # Calculate dimensions
  ncols <- round((xmax - xmin) / res_degrees)
  nrows <- round((ymax - ymin) / res_degrees)
  
  # Create empty raster
  target_grid <- terra::rast(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    ncols = ncols, nrows = nrows,
    crs = "EPSG:4326"
  )
  
  # if (!is.null(crs) & crs != "EPSG:4326"){
  #   target_grid <- project(target_grid, crs)
  # }
  
  return(target_grid)
}
