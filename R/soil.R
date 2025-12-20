# R/soil.R

#' Download and process Harmonized World Soil Database v2.0
#'
#' This function downloads, processes, and extracts variables from the
#' Harmonized World Soil Database v2.0 (HWSD v2.0). The variable corresponds 
#' to a global raster file at 1 km resolution representing soil types.
#'
#' Available variables (working synonyms in parentheses):
#'
#' 1 - "hwsd" ("soil", "type", "soiltype", "soil type")
#'
#' Citation:
#'
#' FAO & IIASA. (2023). "Harmonized World Soil Database v2.0." 
#' Food and Agriculture Organization of the United Nations, Rome and 
#' International Institute for Applied Systems Analysis, Laxenburg, Austria.
#' https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#'        Defaults to "hwsd" if left empty.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' soil(vars=c("soil"))
#'   }
#' @export

soil <- function(x, vars = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Harmonized World Soil Database v2.0.\n",
    "Citation: FAO & IIASA. (2023). Harmonized World Soil Database v2.0.\n",
    "DOI: {.url https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/}\n"
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
  # There is effectively one variable, but we allow multiple synonyms
  soil_lookup <- list(
    "hwsd" = c("soil", "type", "soiltype", "soil type", "hwsd")
  )
  
  # Default to "hwsd" if vars is empty/NULL
  if (is.null(vars)) {
    vars <- "hwsd"
  }
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(soil_lookup)) {
    for (syn in soil_lookup[[canon]]) {
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
      "Unknown HWSD variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/soil")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {user_name}}...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }
    
    # HWSD comes in a zip, so we must unzip it first
    cli::cli_alert_info("Unzipping {.val {basename(dest_file)}}...")
    unzip_dir <- file.path(temp_dir, "unzipped")
    fs::dir_create(unzip_dir)
    utils::unzip(dest_file, exdir = unzip_dir)
    
    # Target the .bil file specifically
    raster_file <- file.path(unzip_dir, "HWSD2.bil")
    
    if (!fs::file_exists(raster_file)) {
      cli::cli_alert_warning("Expected raster file {.val HWSD2.bil} not found in archive.")
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(raster_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {raster_file}}.")
        fs::file_delete(dest_file)
        fs::dir_delete(unzip_dir)
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
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = raster_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        fs::file_delete(dest_file)
        fs::dir_delete(unzip_dir)
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
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  # Since there is only one source file for this function, we define it directly.
  full_url <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip"
  
  cli::cli_alert_info("Starting the download of HWSD data...")
  
  for (canon in requested_codes) {
    # For this specific dataset, the filename/URL is constant regardless of the code
    filename <- "HWSD2_RASTER.zip"
    url <- full_url
    dest <- file.path(fs::path_temp("envar/soil"), filename)
    
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