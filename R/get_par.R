get_par <- function(x){
    
  if (inherits(x, "data.frame")) {
    
    shapefile <- sf::st_as_sf(x, coords = c("X", "Y"), crs=4326)
    x <- process_extent(shapefile)
    return(x)
  }
  
  if (inherits(x, "list")) {
    
    if (x$type=="point"){
      
      x <- x
      return (x)
      
    } else {
      
      grid <- x[[1]]
      mask <- x[[2]]
      res <- x[[3]]
      
      return (list(grid = grid, mask = mask, res = res))
    }
  }
  
  
  if (inherits(x, "SpatRaster")) {
      
      grid = x[[1]]
      
      mask_rs <- !is.na(x)
      
      mask <- sf::st_as_sf(terra::as.polygons(mask_rs, dissolve = TRUE))
      mask = mask[2,]
      terra::plot(mask)
      res = round(terra::res(x)[1]/0.083333333, 0)
      
      return (list(grid = grid, mask = mask, res = res))
    }

}
