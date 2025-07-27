process_points <- function(file, points){

r <- terra::rast(file)
points_1 <- sf::st_transform(points, terra::crs(r))
points_1_vect <- terra::vect(points_1)
extracted <- data.frame(cbind(terra::extract(r, points_1_vect),data.frame(sf::st_coordinates(points_1))))
extracted <- extracted[, c(1, 3, 4, 2)]

return(extracted)
}
