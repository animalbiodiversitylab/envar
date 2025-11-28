# R/accessibility.R

#' Download and process Global Accessibility Indicators
#'
#' `accessibility()` downloads, processes, and extracts variables from the
#' **Global Accessibility Indicators** dataset.
#' Each variable corresponds to a raster representing the travelling time (in minutes)
#' to cities or ports of specific sizes.
#'
#' The function allows users to input either:
#' - **canonical variable codes**, e.g. `"cities1"`, `"ports1"`
#' - **human-readable names**, e.g. `"large cities"`, `"travel time cities 1"`,
#'   `"small ports"`, `"ports 4"`, etc.
#'
#' It automatically:
#' - downloads the selected files,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#' @param x A `SpatRaster`, `SpatVector`, `sf` object, or output from `var_get()` 
#'          defining the area or locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster/polygon input: a `SpatRaster` stack of processed accessibility layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

accessibility <- function(x, vars, ...) {
  
  # Citation displayed on execution
  cli::cli_alert_info(paste0(
    "Using Global Accessibility Indicators.\n",
    "Citation: Nelson, A., Weiss, D.J., van Etten, J. et al. (2019). A suite of global accessibility indicators. Sci Data 6, 266.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-019-0265-5}\n"
  ))
  
  par_list <- get_par(x)
  
  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res <- par_list$res
    crs <- par_list$crs
    is_global <- isTRUE(par_list$is_global)
    is_raster_input <- TRUE
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs <- par_list$crs
    is_raster_input <- FALSE
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # Data Source URLs (Figshare)
  # Note: Accessibility data extent is [-180, 180, -60, 85]
  accessibility_urls <- c(
    "cities1"  = "https://figshare.com/ndownloader/files/14189804",
    "cities2"  = "https://figshare.com/ndownloader/files/14189807",
    "cities3"  = "https://figshare.com/ndownloader/files/14189810",
    "cities4"  = "https://figshare.com/ndownloader/files/14189816",
    "cities5"  = "https://figshare.com/ndownloader/files/14189819",
    "cities6"  = "https://figshare.com/ndownloader/files/14189825",
    "cities7"  = "https://figshare.com/ndownloader/files/14189831",
    "cities8"  = "https://figshare.com/ndownloader/files/14189837",
    "cities9"  = "https://figshare.com/ndownloader/files/14189840",
    "cities10" = "https://figshare.com/ndownloader/files/14189843",
    "cities11" = "https://figshare.com/ndownloader/files/14189849",
    "cities12" = "https://figshare.com/ndownloader/files/14189852",
    "ports1"   = "https://figshare.com/ndownloader/files/14189864",
    "ports2"   = "https://figshare.com/ndownloader/files/14189870",
    "ports3"   = "https://figshare.com/ndownloader/files/14189873",
    "ports4"   = "https://figshare.com/ndownloader/files/14189879",
    "ports5"   = "https://figshare.com/ndownloader/files/14189885"
  )
  
  # Friendly-name -> canonical code mapping
  accessibility_lookup <- list(
    "cities1"  = c("cities 1", "city 1", "cities >5m", "huge cities", "travel time cities 1"),
    "cities2"  = c("cities 2", "city 2", "cities >1m", "large cities", "travel time cities 2"),
    "cities3"  = c("cities 3", "city 3", "medium cities", "travel time cities 3"),
    "cities4"  = c("cities 4", "city 4", "small cities", "travel time cities 4"),
    "cities5"  = c("cities 5", "city 5", "travel time cities 5"),
    "cities6"  = c("cities 6", "city 6", "travel time cities 6"),
    "cities7"  = c("cities 7", "city 7", "travel time cities 7"),
    "cities8"  = c("cities 8", "city 8", "towns", "travel time cities 8"),
    "cities9"  = c("cities 9", "city 9", "small towns", "travel time cities 9"),
    "cities10" = c("cities 10", "city 10", "aggregated cities 1", "travel time cities 10"),
    "cities11" = c("cities 11", "city 11", "aggregated cities 2", "travel time cities 11"),
    "cities12" = c("cities 12", "city 12", "aggregated cities 3", "travel time cities 12"),
    "ports1"   = c("ports 1", "port 1", "large ports", "travel time ports 1"),
    "ports2"   = c("ports 2", "port 2", "medium ports", "travel time ports 2"),
    "ports3"   = c("ports 3", "port 3", "small ports", "travel time ports 3"),
    "ports4"   = c("ports 4", "port 4", "very small ports", "travel time ports 4"),
    "ports5"   = c("ports 5", "port 5", "any port", "all ports", "travel time ports 5")
  )
  
  # Normalizer
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(accessibility_lookup)) {
    for (syn in accessibility_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes
  requested_codes <- character(0)
  unmapped <- character(0)
  
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      requested_codes <- c(requested_codes, syn2canon[[key]])
    } else {
      unmapped <- c(unmapped, v)
    }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c(
      "Unknown Accessibility variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # Helper: Download, process, and clean up a single file
  handle_file <- function(url, dest_file, var) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {var}}...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {var}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {basename(dest_file)}}...")
      
      # Process layer based on whether we're doing global or regional processing
      layer1 <- process_raster_layer(
        layer = layer,
        grid = grid,
        mask = mask,
        res = res,
        crs = crs,
        is_global = is_global
      )
      
      # Assign name to layer
      names(layer1) <- var
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {basename(dest_file)}} to stack.")
      
      rm(layer, layer1)
      gc()
      fs::file_delete(dest_file)
      
    } else {
      # Point extraction
      cli::cli_alert_info("Extracting values from {.val {basename(dest_file)}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {basename(dest_file)}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
        names(extracted)[ncol(extracted)] <- var
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {basename(dest_file)}} successfully.")
      
      rm(extracted)
      gc()
      fs::file_delete(dest_file)
    }
  }
  
  # Loop through requested variables
  cli::cli_alert_info("Starting the download of Accessibility data...")
  
  for (canon in requested_codes) {
    url <- accessibility_urls[[canon]]
    
    if (is.null(url)) {
      cli::cli_alert_warning("No URL found for {.val {canon}}.")
      next
    }
    
    filename <- paste0(canon, ".tif")
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
    handle_file(url = url, dest_file = dest, var = canon)
  }
  
  # Return output
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    # If x was already a SpatRaster (from previous function), combine
    if (inherits(x, "SpatRaster")) {
      processed_stack <- c(x, processed_stack)
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}

#' Process a raster layer according to global or regional settings
#' @noRd
process_raster_layer <- function(layer, grid, mask, res, crs, is_global = FALSE) {
  
  source_crs <- terra::crs(layer)
  target_crs <- crs
  
  # Check if target is geographic
  is_target_geographic <- tryCatch({
    sf::st_crs(target_crs)$IsGeographic
  }, error = function(e) {
    grepl("EPSG:4326|WGS.*84|longlat|latlong", target_crs, ignore.case = TRUE)
  })
  
  if (is_global) {
    # Global processing: keep original extent, apply resolution and CRS
    
    # First, aggregate if needed
    if (res > 1) {
      cli::cli_alert_info("Aggregating by factor {res}...")
      layer <- terra::aggregate(layer, fact = res, fun = "mean", na.rm = TRUE)
    }
    
    # Project to target CRS if different
    if (target_crs != "EPSG:4326" && !grepl("4326", target_crs)) {
      cli::cli_alert_info("Projecting to {target_crs}...")
      layer <- terra::project(layer, target_crs, method = "bilinear")
    }
    
    return(layer)
    
  } else {
    # Regional processing: crop, resample, mask
    
    # Convert mask to target CRS if needed
    mask_crs <- sf::st_crs(mask)
    if (!is.na(mask_crs) && !identical(mask_crs, sf::st_crs(source_crs))) {
      mask_reproj <- sf::st_transform(mask, source_crs)
    } else {
      mask_reproj <- mask
    }
    
    # Check if source and target CRS differ
    source_crs_wkt <- terra::crs(layer)
    target_crs_wkt <- terra::crs(grid)
    crs_differ <- !identical(source_crs_wkt, target_crs_wkt)
    
    if (crs_differ) {
      # Different CRS: project grid to source, crop, then project back
      grid_reproj <- terra::project(grid, source_crs_wkt)
      
      # Crop to reprojected grid extent
      layer_cropped <- terra::crop(layer, grid_reproj, snap = "out")
      
      # Resample to reprojected grid
      layer_resampled <- terra::resample(layer_cropped, grid_reproj, method = "bilinear")
      
      # Mask with reprojected mask
      mask_vect <- terra::vect(mask_reproj)
      layer_masked <- terra::mask(layer_resampled, mask_vect)
      
      # Project to target CRS
      layer_final <- terra::project(layer_masked, target_crs_wkt, method = "bilinear")
      
    } else {
      # Same CRS: straightforward crop, resample, mask
      layer_cropped <- terra::crop(layer, grid, snap = "out")
      layer_resampled <- terra::resample(layer_cropped, grid, method = "bilinear")
      
      mask_vect <- terra::vect(mask)
      layer_final <- terra::mask(layer_resampled, mask_vect)
    }
    
    return(layer_final)
  }
}