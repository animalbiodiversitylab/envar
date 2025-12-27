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
        global_extent = x$global_extent,  # Track cumulative global extent
        from_varget = TRUE,
        set_na= x$set_na,
        path = x$path,
        land = x$land
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
        from_varget = TRUE,
        set_na=x$set_na,
        path = x$path
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
      global_extent = x$global_extent,  # Track cumulative global extent
      from_varget = TRUE,
      set_na = x$set_na,
      path = x$path,
      land = x$land
    ))
  }
  
  # If it inherits a raster from a previous download and crop:
  if (inherits(x, "SpatRaster")) {
    
    # Check for global extent attribute
    global_extent <- attr(x, "global_extent")
    is_global <- isTRUE(attr(x, "is_global"))
    
    # extract attributes of path, set_na and land
    path <- attr(x, "path")
    set_na <- attr(x, "set_na")
    land <- attr(x, "land")
    
    if (is_global){
      grid <- x[[1]]
      extent <- terra::ext(grid)
      raster_crs <- terra::crs(grid, describe = TRUE)
      crs_string <- terra::crs(grid)
      
      # Create a clean grid template instead of deriving unreliable mask
      grid_template <- terra::rast(
        extent = terra::ext(grid),
        resolution = terra::res(grid),
        crs = crs_string
      )
      terra::values(grid_template) <- 1
      
      # Determine resolution multiplier
      raster_res <- terra::res(x)[1]
      
      # Check if geographic or projected
      is_geographic <- tryCatch({
        sf::st_crs(crs_string)$IsGeographic
      }, error = function(e) {
        raster_res < 1
      })
      
      if (isTRUE(is_geographic)) {
        res <- round(raster_res / 0.008333333, 0)
      } else {
        res <- round(raster_res / 1000, 0)
      }
      res <- max(res, 1)
      
      # Check for global extent attribute
      global_extent <- attr(x, "global_extent")
      is_global <- isTRUE(attr(x, "is_global"))
      
      return(list(
        grid = grid_template,
        mask = NULL,
        res = res,
        crs = crs_string,
        type = "polygon",
        is_global = is_global,
        global_extent = global_extent,
        from_varget = FALSE,
        set_na=set_na,
        path = path,
        land = land
      ))
      
    } else {
    
    grid <- x[[1]]
    extent <- terra::ext(grid)
    raster_crs <- terra::crs(grid, describe = TRUE)
    crs_string <- terra::crs(grid)
    
    mask_rs <- !is.na(x)
    
    mask <- sf::st_as_sf(terra::as.polygons(mask_rs, dissolve = TRUE))
    mask = mask[2,]
    
    
    
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
      is_global = is_global,
      global_extent = global_extent,
      from_varget = FALSE,
      set_na=set_na,
      path=path,
      land=land
    ))
    }
  }  
  
  # If it inherits a dataframe from a previous point extraction:
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    
    path <- attr(x, "path")
      
    # Check for coordinate columns
    coord_cols <- c("X", "Y")
    if (!all(coord_cols %in% names(x))) {
      cli::cli_abort("Data frame must contain 'X' and 'Y' columns for coordinates.")
    }
    
    # FIXED: Check for stored CRS attribute from previous extraction
    # This preserves the original CRS through the chain
    stored_crs <- attr(x, "envar_crs")
    
    if (!is.null(stored_crs)) {
      # Use the stored CRS from previous extraction
      point_crs <- stored_crs
    } else {
      # Fallback: try to detect CRS from coordinate values
      max_x <- max(abs(x$X), na.rm = TRUE)
      max_y <- max(abs(x$Y), na.rm = TRUE)
      
      if (max_x > 180 || max_y > 90) {
        # Coordinates are too large for WGS84, likely projected
        cli::cli_alert_warning("No CRS stored in data.frame. Coordinates appear projected (X={round(max_x)}, Y={round(max_y)}).")
        
        # Check if coordinates are in typical EPSG:3035 range (European LAEA)
        if (max_x > 1000000 && max_x < 8000000 && max_y > 1000000 && max_y < 6000000) {
          point_crs <- "EPSG:3035"
          cli::cli_alert_info("Detected likely EPSG:3035 (European LAEA) based on coordinate range.")
        } else {
          cli::cli_alert_warning("Could not detect CRS. Assuming EPSG:4326 - results may be incorrect!")
          point_crs <- "EPSG:4326"
        }
      } else {
        point_crs <- "EPSG:4326"
      }
    }
    
    # Create sf object with the correct CRS
    shapefile <- sf::st_as_sf(x, coords = c("X", "Y"), crs = point_crs)
    
    # Process extent with the correct CRS
    extent_info <- process_extent(shapefile, crs = point_crs)
    extent_info$crs <- point_crs
    extent_info$from_varget <- FALSE
    extent_info$path <- path
      
    return(extent_info)
  }
  
  cli::cli_abort("Unsupported input type for get_par()")
}