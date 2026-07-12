# R/earthenvlandcover.R

#' Download and process EarthEnv land cover variables
#'
#' This function downloads, processes, and extracts variables from the
#' EarthEnv Consensus Land Cover dataset. Each variable corresponds to a global
#' raster representing a specific land cover class at 1-km resolution.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item 1 - "consensus_full_class_1" ("evergreen deciduous needleleaf trees", "needleleaf trees", "needleleaf", "conifer")
#'   \item 2 - "consensus_full_class_2" ("evergreen broadleaf trees", "evergreen broadleaf", "broadleaf evergreen")
#'   \item 3 - "consensus_full_class_3" ("deciduous broadleaf trees", "deciduous broadleaf", "broadleaf deciduous")
#'   \item 4 - "consensus_full_class_4" ("mixed other trees", "mixed trees", "other trees", "mixed forest")
#'   \item 5 - "consensus_full_class_5" ("shrubs", "shrubland", "shrub")
#'   \item 6 - "consensus_full_class_6" ("herbaceous vegetation", "herbaceous", "grassland", "grass", "herbs")
#'   \item 7 - "consensus_full_class_7" ("cultivated and managed vegetation", "cultivated", "managed vegetation", "agriculture", "crops", "cropland")
#'   \item 8 - "consensus_full_class_8" ("regularly flooded vegetation", "flooded vegetation", "flooded", "wetland")
#'   \item 9 - "consensus_full_class_9" ("urban built up", "urban", "built up", "built-up", "artificial surface")
#'   \item 10 - "consensus_full_class_10" ("snow ice", "snow", "ice", "glacier", "permafrost")
#'   \item 11 - "consensus_full_class_11" ("barren", "barren land", "bare ground", "bare")
#'   \item 12 - "consensus_full_class_12" ("open water", "water", "water bodies")
#' }
#'
#' \strong{Citation:}\cr
#' Tuanmu MN, Jetz W (2014). "A global 1-km consensus land-cover product for biodiversity and ecosystem modeling." Global Ecology and Biogeography 23, 1031-1045.
#' https://doi.org/10.1111/geb.12182
#'
#' Note: Users should verify the terms of use for EarthEnv data provided 
#' at https://www.earthenv.org/
#' 
#' @param x The output from `par_set()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param discover Logical. If `TRUE` (default), downloads the version integrated 
#'        with the DISCover dataset. If `FALSE`, downloads the version without 
#'        DISCover integration.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \donttest{
#' processed <- par_set(country= "Italy", crs=3035) %>% 
#' earthenvlandcover(vars=c("snow ice"))
#'   }
#' @export

earthenvlandcover <- function(x, vars, discover = TRUE, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  
  cli::cli_alert_info(paste0(
    "Using EarthEnv Consensus Land Cover layers.\n",
    "Citation: Tuanmu MN, Jetz W (2014). A global 1-km consensus land-cover product for biodiversity and ecosystem modeling. Global Ecology and Biogeography 23, 1031-1045.\n",
    "DOI: {.url https://doi.org/10.1111/geb.12182}\n"
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
  fn_env <- environment()
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  earthenv_lookup <- list(
    "consensus_full_class_1"  = c("evergreen deciduous needleleaf trees", "needleleaf trees", "needleleaf", "conifer"),
    "consensus_full_class_2"  = c("evergreen broadleaf trees", "evergreen broadleaf", "broadleaf evergreen"),
    "consensus_full_class_3"  = c("deciduous broadleaf trees", "deciduous broadleaf", "broadleaf deciduous"),
    "consensus_full_class_4"  = c("mixed other trees", "mixed trees", "other trees", "mixed forest"),
    "consensus_full_class_5"  = c("shrubs", "shrubland", "shrub"),
    "consensus_full_class_6"  = c("herbaceous vegetation", "herbaceous", "grassland", "grass", "herbs"),
    "consensus_full_class_7"  = c("cultivated and managed vegetation", "cultivated", "managed vegetation", "agriculture", "crops", "cropland"),
    "consensus_full_class_8"  = c("regularly flooded vegetation", "flooded vegetation", "flooded", "wetland"),
    "consensus_full_class_9"  = c("urban built up", "urban", "built up", "built-up", "artificial surface"),
    "consensus_full_class_10" = c("snow ice", "snow", "ice", "glacier", "permafrost"),
    "consensus_full_class_11" = c("barren", "barren land", "bare ground", "bare"),
    "consensus_full_class_12" = c("open water", "water", "water bodies")
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
  for (canon in names(earthenv_lookup)) {
    for (syn in earthenv_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes and keep mapping to original names
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
      "Unknown EarthEnv variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- envar_grids_dir()
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
        fn_env$current_global_extent <- new_extent
        
        # If we have existing layers and extent changed, crop them
        if (!is.null(processed_stack)) {
          fn_env$processed_stack <- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        # For regional processing, result is just the layer
        layer1 <- result
      }
      
      # Assign user-requested name to layer
      names(layer1) <- user_name
      
      if (is.null(processed_stack)) {
        fn_env$processed_stack <- layer1
      } else {
        fn_env$processed_stack <- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) {
        #fs::file_delete(dest_file)
      }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
        names(extracted)[ncol(extracted)] <- user_name
      }
      
      if (is.null(extracted_df)) {
        fn_env$extracted_df <- extracted
      } else {
        fn_env$extracted_df <- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      
      rm(extracted)
      gc()
      if (!is_global) {
        #fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  if (discover) {
    base_url <- "https://data.earthenv.org/consensus_landcover/with_DISCover"
  } else {
    base_url <- "https://data.earthenv.org/consensus_landcover/without_DISCover"
  }
  
  cli::cli_alert_info("Starting the download of EarthEnv Consensus Land Cover data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- file.path(base_url, filename)
    
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
    attr(processed_stack, "land")<-land
    
    # Remove NAs if necessary
    if (set_na==TRUE){
      
      cli::cli_alert_info("Applying NA mask...")
      
      master_mask <- sum(processed_stack)
      # Apply that master mask to the whole stack
      processed_stack <- terra::mask(processed_stack, master_mask)
      
    }
    
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
    
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
