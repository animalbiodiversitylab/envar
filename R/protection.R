# R/protection.R

#' Download and process WDPA Protected Area layers
#'
#' This function downloads, processes, and extracts variables from the
#' World Database of Protected Areas (WDPA). Each variable corresponds 
#' to a global raster representing different IUCN Management Categories of 
#' protected areas.
#'
#' Available variables (working synonyms in parentheses):
#'
#' 1 - "WDPA_IA" ("strict nature reserve", "strict reserve", "1a", "ia", "Ia")
#' 
#' 2 - "WDPA_IB" ("wilderness area", "wilderness", "1b", "ib", "Ib")
#' 
#' 3 - "WDPA_II" ("national park", "park", "2", "ii", "II")
#' 
#' 4 - "WDPA_III" ("natural monument", "monument", "3", "iii", "III")
#' 
#' 5 - "WDPA_IV" ("habitat species management", "habitat management", "4", "iv", "IV")
#' 
#' 6 - "WDPA_V" ("protected landscape", "protected seascape", "landscape", "5", "v", "V")
#' 
#' 7 - "WDPA_VI" ("sustainable use", "natural resources", "6", "vi", "VI")
#' 
#' 8 - "WDPA_ALL" ("all", "combined", "full", "total", "all protected areas")
#'
#' Citation:
#'
#' Protected Planet (2025). "World Database of Protected Areas (WDPA)."
#' https://www.protectedplanet.net/en
#'
#' Note: Users should ensure they comply with the terms of use of the WDPA
#' when using these data for commercial or research purposes.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' protection(vars=c("national park", "WDPA_ALL"))
#'   }
#' @export

protection <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using World Database of Protected Areas (WDPA) layers.\n",
    "Citation: Protected Planet (2025). World Database of Protected Areas (WDPA).\n",
    "DOI: {.url https://www.protectedplanet.net/en}\n"
  ))
  
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
  protection_lookup <- list(
    "WDPA_IA"  = c("strict nature reserve", "strict reserve", "1a", "ia", "Ia"),
    "WDPA_IB"  = c("wilderness area", "wilderness", "1b", "ib", "Ib"),
    "WDPA_II"  = c("national park", "park", "2", "ii", "II"),
    "WDPA_III" = c("natural monument", "monument", "3", "iii", "III"),
    "WDPA_IV"  = c("habitat species management", "habitat management", "4", "iv", "IV"),
    "WDPA_V"   = c("protected landscape", "protected seascape", "landscape", "5", "v", "V"),
    "WDPA_VI"  = c("sustainable use", "natural resources", "6", "vi", "VI"),
    "WDPA_ALL" = c("all", "combined", "full", "total", "all protected areas")
  )
  
  # Direct URL lookup
  url_lookup <- list(
    "WDPA_IA"  = "https://figshare.com/ndownloader/files/59746952?private_link=f0cabc378ea496838f66",
    "WDPA_IB"  = "https://figshare.com/ndownloader/files/59746949?private_link=f0cabc378ea496838f66",
    "WDPA_II"  = "https://figshare.com/ndownloader/files/59746562?private_link=f0cabc378ea496838f66",
    "WDPA_III" = "https://figshare.com/ndownloader/files/59746559?private_link=f0cabc378ea496838f66",
    "WDPA_IV"  = "https://figshare.com/ndownloader/files/59746565?private_link=f0cabc378ea496838f66",
    "WDPA_V"   = "https://figshare.com/ndownloader/files/59746568?private_link=f0cabc378ea496838f66",
    "WDPA_VI"  = "https://figshare.com/ndownloader/files/59746571?private_link=f0cabc378ea496838f66",
    "WDPA_ALL" = "https://figshare.com/ndownloader/files/59747045?private_link=f0cabc378ea496838f66"
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
  for (canon in names(protection_lookup)) {
    for (syn in protection_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
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
      "Unknown WDPA variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {user_name}}...")
    
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
          fs::file_delete(dest_file)
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
      names(layer1) <- user_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) {
        fs::file_delete(dest_file)
      }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
          fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
        names(extracted)[ncol(extracted)] <- user_name
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
        fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of WDPA data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- url_lookup[[canon]]
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    
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
          processed_stack <- terra::resample(processed_stack, x, method = "bilinear")
          
        }
        processed_stack <- c(x, processed_stack)
      }
      
      
    }
    
    # Attach global extent as attribute for downstream functions
    if (is_global) {
      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
    }
    attr(processed_stack, "set_na") <- set_na
    attr(processed_stack, "path") <- path
    
    
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