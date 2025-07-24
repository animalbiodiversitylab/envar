get_par <- function(x){
    if (is.list(x)) {
      grid <- x[[1]]
      mask <- x[[2]]
      res <- x[[3]]
      
    } else {
      grid = x[[1]]
      
      mask_rs <- !is.na(x)
      
      mask <- sf::st_as_sf(terra::as.polygons(mask_rs, dissolve = TRUE))
      mask = mask[2,]
      terra::plot(mask)
      res = round(terra::res(x)[1]/0.083333333, 0)
    }
  return (list(grid = grid, mask = mask, res = res))
}
