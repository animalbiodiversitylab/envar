# R/hybridlandcover.R

#' Download and process Hybrid Global Annual 1-km IGBP Land Cover Maps
#'
#' This function downloads, processes, and extracts land cover variables from the
#' Hybrid Global Annual 1-km IGBP Land Cover Maps dataset (Luo et al., 2024).
#' The data covers the period from 2000 to 2020.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item 1 - "landcover" ("cover", "land", "lc", "igbp", "hybrid")
#' }
#'
#' \strong{Citation:}\cr
#' Luo Y, Zhu Z, Zhao W, Li M, Chen J, Zhao P, Sun L, Zhang Y, Duanmu Z, Chen J (2024). "Hybrid Global Annual 1-km IGBP Land Cover Maps for the Period 2000–2020." Journal of Remote Sensing, 4, 0122.
#' https://doi.org/10.34133/remotesensing.0122
#'
#' Note: You must specify the `year` argument (integer between 2000 and 2020).
#'
#' @param x The output from `par_set()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param year Integer. The year for which to download the land cover map (2000-2020). Defaults to 2000.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- par_set(country= "Italy", crs=3035) %>% 
#'   hybridlandcover(vars="landcover", year=2015)
#'   }
#' @export
hybridlandcover <- function(x, vars = NULL, year = 2000, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Hybrid Global Annual 1-km IGBP Land Cover Maps.\n",
    "Citation: Luo Y, Zhu Z, Zhao W, Li M, Chen J, Zhao P, Sun L, Zhang Y, Duanmu Z, Chen J (2024). Hybrid Global Annual 1-km IGBP Land Cover Maps for the Period 2000\u20132020. Journal of Remote Sensing, 4, 0122.\n",
    "DOI: {.url https://doi.org/10.34133/remotesensing.0122}\n"
  ))
  
  # Validate Year
  if (!is.numeric(year) || year < 2000 || year > 2020) {
    cli::cli_abort("Year must be an integer between 2000 and 2020.")
  }
  
  par_list <- get_par(x)
  
  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res  <- par_list$res
    crs  <- par_list$crs
    is_global <- isTRUE(par_list$is_global)
    is_raster_input <- TRUE
    set_na=par_list$set_na
    path = par_list$path
    land = par_list$land
    # Track cumulative global extent
    current_global_extent <- par_list$global_extent
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs  <- par_list$crs
    is_global <- FALSE
    is_raster_input <- FALSE
    current_global_extent <- NULL
    path = par_list$path
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  # Since the variable is just "landcover" regardless of year, we map it to a generic code
  # The actual file download URL will depend on the 'year' argument later
  land_lookup <- list(
    "landcover" = c("landcover", "cover", "land", "lc", "igbp", "hybrid")
  )
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(land_lookup)) {
    for (syn in land_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Handle empty vars argument (default to downloading the map)
  if (is.null(vars) || length(vars) == 0 || all(vars == "")) {
    vars <- "landcover"
  }
  
  # Convert requested vars to canonical codes AND keep mapping to original names
  requested_codes <- character(0)
  code_to_user_name <- list() # Maps canonical code -> user's original name
  unmapped <- character(0)
  
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      canon <- syn2canon[[key]]
      # Only add if not already present (avoid duplicates)
      if (!(canon %in% requested_codes)) {
        requested_codes <- c(requested_codes, canon)
        # Store the user's original name for this canonical code
        code_to_user_name[[canon]] <- v
      }
    } else {
      unmapped <- c(unmapped, v)
    }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c(
      "Unknown Land Cover variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/hybridlandcover")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {user_name}} (Year {year})...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {user_name}}...")
      
      # Process layer using standard helper
      result <- process_raster_layer(
        layer = layer,
        grid = grid,
        mask = mask,
        res = res,
        crs = crs,
        is_global = is_global,
        current_extent = current_global_extent
      )
      
      if (is_global) {
        # For global processing, result is a list with layer and extent
        layer1 <- result$layer
        new_extent <- result$extent
        
        # Update the cumulative global extent
        current_global_extent <<- new_extent
        
        # If we have existing layers and extent changed, crop them
        if (!is.null(processed_stack)) {
          processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        # For regional processing, result is just the layer
        layer1 <- result
      }
      
      # Assign user-requested name to layer
      # Append year to name to clarify which year was downloaded
      names(layer1) <- paste0(user_name, "_", year)
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) {
        # fs::file_delete(dest_file)
      }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
          # fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column (append year)
        names(extracted)[ncol(extracted)] <- paste0(user_name, "_", year)
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      
      rm(extracted)
      gc()
      if (!is_global) {
        #   fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  
  cli::cli_alert_info("Processing Hybrid Land Cover data...")
  
  for (canon in requested_codes) {
    # Construct filename and URL dynamically based on the requested year
    filename <- paste0("HYBMAP_IGBP_", year, "_LC.tif")
    
    # Base Zenodo URL pattern
    # 2000: https://zenodo.org/records/10488191/files/HYBMAP_IGBP_2000_LC.tif?download=1
    
    base_url <- "https://zenodo.org/records/10488191/files/"
    url <- paste0(base_url, filename, "?download=1")
    
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    dest <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
    
    handle_file(url, dest, canon, user_name)
  }
  
  # --------------------------------------------------------------------
  # Return output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    # If x was already a SpatRaster (from previous function), combine
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(
          existing_stack = x,
          new_stack = processed_stack,
          current_global_extent = current_global_extent
        )
      } else {
        # Regional mode: resample new layers to match input raster exactly
        # This ensures perfect alignment for stacking
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = choose_resample_method(processed_stack))
          
        }
        processed_stack <- c(x, processed_stack)
      }
      
      
    }
    
    # Attach global extent as attribute for downstream functions
    if (is_global) {
      
      if (land == TRUE){
        cli::cli_alert_info(paste0(
          "Global masking with land boundary from Natural Earth database...\n",
          "Website: {.url https://www.naturalearthdata.com/}\n"
        ))
        invisible(capture.output(suppressMessages(suppressWarnings(land_sf <- rnaturalearth::ne_download(
          scale = "medium",
          type = "land",
          category = "physical",
          returnclass = "sf")))))
        
        processed_stack <-terra::crop(terra::mask(processed_stack, land_sf), land_sf)
      }
      
      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
    }
    
    attr(processed_stack, "set_na") <- set_na
    attr(processed_stack, "path") <- path
    attr(processed_stack, "land") <- land
    
    # remove NAs if necessary
    if (set_na==TRUE){
      
      cli::cli_alert_info("Applying NA mask...")
      
      master_mask <- sum(processed_stack)
      # Apply that master mask to the whole stack
      processed_stack <- terra::mask(processed_stack, master_mask)
      
    }
    
    # write if requested
    
    if (!is.null(path)){
      terra::writeRaster(processed_stack, path, overwrite = TRUE)
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    # Merge with previous data if x was a data.frame
    if (inherits(x, "data.frame") && !inherits(x, "sf")) {
      extracted_df <- merge(x, extracted_df[, c(1, 4:ncol(extracted_df))], by = c("ID"), all = TRUE)
      # Preserve CRS from previous extraction
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) {
        crs <- prev_crs
      }
    }
    
    # Store the CRS as an attribute for downstream functions
    # This ensures the CRS is preserved when chaining point extractions
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path
    
    # write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
