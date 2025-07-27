get_par <- function(x){
    

  # If it inherits (from var_get) a list and the first element is a SpatRaster
  if (inherits(x, "list") & inherits(x[[1]], "SpatRaster")) {
      
      grid <- x[[1]]
      mask <- x[[2]]
      res <- x[[3]]
      
      return (list(grid = grid, mask = mask, res = res))

  }
  
# If it inherits (from var_get) a list and the first element is type = "point"
  if (inherits(x, "list") & (!inherits(x[[1]], "SpatRaster"))) {
      
      x <- x
      return (x)
      
  }
  
  # 3. If it inherits a raster from a previous download and crop:
  
  if (inherits(x, "SpatRaster")) {
      
      grid = x[[1]]
      
      mask_rs <- !is.na(x)
      
      mask <- sf::st_as_sf(terra::as.polygons(mask_rs, dissolve = TRUE))
      mask = mask[2,]
      terra::plot(mask)
      res = round(terra::res(x)[1]/0.083333333, 0)
      
      return (list(grid = grid, mask = mask, res = res))
  }
  
  # 4. If it inherits a dataframe from a previous point extraction:
  
  if (inherits(x, "data.frame")) {
    
    shapefile <- sf::st_as_sf(x, coords = c("X", "Y"), crs=4326)
    x <- process_extent(shapefile)
    return(x)
  }
  
  

}
