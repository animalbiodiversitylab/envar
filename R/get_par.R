# R/get_par.R
#' Get parameters from input
#' @noRd
get_par <- function(x) {
  
  # If it inherits from var_get (has class envar_par) and is a list
  if (inherits(x, "envar_par") || (inherits(x, "list") && isTRUE(x$from_varget))) {
    
    # Check if it's raster-based (has grid) or point-based
    if (!is.null(x$grid) && inherits(x$grid, "SpatRaster")) {
      return(list(
        grid = x$grid,
        mask = x$mask,
        res = x$res,
        crs = x$crs,
        type = x$type,
        is_global = isTRUE(x$is_global),
        from_varget = TRUE
      ))
    }
    
    # Point-based input
    if (x$type == "point") {
      return(list(
        type = "point",
        mask = x$mask,
        bbox = x$bbox,
        crs = x$crs,
        res = x$res,
        is_global = FALSE,
        from_varget = TRUE
      ))
    }
  }
  
  # If it inherits a list with first element being a SpatRaster (from previous data function)
  if (inherits(x, "list") && !is.null(x[[1]]) && inherits(x[[1]], "SpatRaster")) {
    grid <- x[[1]]
    mask <- x[[2]]
    res <- x[[3]]
    crs <- x$crs
    
    return(list(
      grid = grid,
      mask = mask,
      res = res,
      crs = crs,
      type = "polygon",
      is_global = isTRUE(x$is_global),
      from_varget = TRUE
    ))
  }
  
  # If it inherits a raster from a previous download and crop:
  if (inherits(x, "SpatRaster")) {
    grid <- x[[1]]
    extent <- terra::ext(grid)
    raster_crs <- terra::crs(grid, describe = TRUE)
    crs_string <- terra::crs(grid)
    
    mask <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(
      xmin = as.numeric(extent[1]),
      ymin = as.numeric(extent[3]),
      xmax = as.numeric(extent[2]),
      ymax = as.numeric(extent[4])
    ), crs = sf::st_crs(crs_string))))
    
    # Determine resolution multiplier
    raster_res <- terra::res(x)[1]
    
    # Check if geographic or projected
    is_geographic <- tryCatch({
      sf::st_crs(crs_string)$IsGeographic
    }, error = function(e) {
      # Fallback: assume geographic if resolution is very small
      raster_res < 1
    })
    
    if (isTRUE(is_geographic)) {
      res <- round(raster_res / 0.008333333, 0)
    } else {
      res <- round(raster_res / 1000, 0)
    }
    res <- max(res, 1)
    
    return(list(
      grid = grid,
      mask = mask,
      res = res,
      crs = crs_string,
      type = "polygon",
      is_global = FALSE,
      from_varget = FALSE
    ))
  }
  
  # If it inherits a dataframe from a previous point extraction:
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    # Check for coordinate columns
    coord_cols <- c("X", "Y")
    if (!all(coord_cols %in% names(x))) {
      cli::cli_abort("Data frame must contain 'X' and 'Y' columns for coordinates.")
    }
    
    shapefile <- sf::st_as_sf(x, coords = c("X", "Y"), crs = 4326)
    extent_info <- process_extent(shapefile, crs = "EPSG:4326")
    extent_info$from_varget <- FALSE
    
    return(extent_info)
  }
  
  cli::cli_abort("Unsupported input type for get_par()")
}