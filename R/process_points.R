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
  
  # Combine results
  result <- data.frame(
    ID = seq_len(nrow(coords)),
    X = coords[, "X"],
    Y = coords[, "Y"],
    value = extracted[, 2]  # Column 2 is the extracted value (column 1 is ID)
  )
  
  # Name the value column with the raster layer name
  names(result)[4] <- names(r)
  
  return(result)
}