# R/process_points.R

#' Extract raster values at point locations
#' @param file Path to raster file
#' @param points sf object containing point geometries
#' @return data.frame with ID, X, Y, and extracted values
#' @noRd
process_points <- function(file, points) {
  
  # Read raster
  r <- terra::rast(file)
  
  # Ensure points are sf
  if (!inherits(points, "sf")) {
    cli::cli_abort("points must be an sf object")
  }
  
  # Transform points to raster CRS for accurate extraction
  raster_crs <- terra::crs(r)
  points_transformed <- sf::st_transform(points, raster_crs)
  
  # Convert to terra vect
  points_vect <- terra::vect(points_transformed)
  
  # Extract values
  extracted <- terra::extract(r, points_vect)
  
  # Get coordinates in original CRS
  coords <- sf::st_coordinates(points)
  
  # Combine results — keep all value columns (multi-band rasters have many)
  val_cols <- extracted[, -1, drop = FALSE]  # drop the ID column (column 1)
  result <- data.frame(
    ID = seq_len(nrow(coords)),
    X = coords[, "X"],
    Y = coords[, "Y"],
    val_cols,
    check.names = FALSE
  )

  # Name the value columns with the raster layer names
  names(result)[-(1:3)] <- names(r)
  
  return(result)
}