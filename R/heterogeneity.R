# R/heterogeneity.R

#' Download and process EarthEnv habitat heterogeneity layers
#'
#' This function downloads, processes, and extracts variables from the
#' EarthEnv habitat heterogeneity dataset (1-km resolution). Each variable 
#' corresponds to a global Cloud-Optimized GeoTIFF (COG) representing different 
#' metrics of habitat heterogeneity derived from remote sensing data.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{First-order statistics}
#' \itemize{
#'   \item "cv" ("coefficient of variation", "coeff of variation")
#'   \item "evenness" ("even")
#'   \item "range"
#'   \item "shannon" ("shannon index", "shannon entropy")
#'   \item "simpson" ("simpson index", "simpson diversity")
#'   \item "std" ("standard deviation", "std dev")
#' }
#' 
#' \strong{Second-order statistics (texture metrics)}
#' \itemize{
#'   \item "Contrast" ("contrast")
#'   \item "Correlation" ("correlation", "corr")
#'   \item "Dissimilarity" ("dissimilarity")
#'   \item "Entropy" ("entropy", "texture entropy")
#'   \item "Homogeneity" ("homogeneity")
#'   \item "Maximum" ("maximum", "max")
#'   \item "Uniformity" ("uniformity", "uniform")
#'   \item "Variance" ("variance", "var")
#' }
#'
#' \strong{Citation:}\cr
#' Tuanmu, M.-N. & Jetz, W. (2015). "A global, remote sensing-based characterization 
#' of terrestrial habitat heterogeneity for biodiversity and ecosystem modeling." 
#' Global Ecology and Biogeography, 24(11), 1329-1339.
#' https://doi.org/10.1111/geb.12365
#'
#' Note: Please cite original sources of primary datasets where appropriate.
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
#' processed <- var_get(shape = Alps, crs = 3035) %>% 
#' heterogeneity(vars=c("shannon", "cv"))
#'   }
#' @export

heterogeneity <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Habitat Heterogeneity layers (1-km).\n",
    "Citation: Tuanmu MN, Jetz W (2015). A global, remote sensing-based characterization of terrestrial habitat heterogeneity... Global Ecology and Biogeography.\n",
    "DOI: {.url https://doi.org/10.1111/geb.12365}\n"
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
  # The canonical code is the short, variable-defining part of the URL/filename.
  # --------------------------------------------------------------------
  # Full URL components (variable part -> full filename part)
  hetero_url_components <- list(
    "cv"            = "cv_01_05_1km_uint16.tif",
    "evenness"      = "evenness_01_05_1km_uint16.tif",
    "range"         = "range_01_05_1km_uint16.tif",
    "shannon"       = "shannon_01_05_1km_uint16.tif",
    "simpson"       = "simpson_01_05_1km_uint16.tif",
    "std"           = "std_01_05_1km_uint16.tif",
    "Contrast"      = "Contrast_01_05_1km_uint32.tif",
    "Correlation"   = "Correlation_01_05_1km_int16.tif",
    "Dissimilarity" = "Dissimilarity_01_05_1km_uint32.tif",
    "Entropy"       = "Entropy_01_05_1km_uint16.tif",
    "Homogeneity"   = "Homogeneity_01_05_1km_uint16.tif",
    "Maximum"       = "Maximum_01_05_1km_uint16.tif",
    "Uniformity"    = "Uniformity_01_05_1km_uint16.tif",
    "Variance"      = "Variance_01_05_1km_uint32.tif"
  )
  
  # Friendly-name -> canonical code mapping (canonical code is the key in hetero_url_components)
  hetero_lookup <- list(
    "cv"            = c("coefficient of variation", "coeff of variation", "cv"),
    "evenness"      = c("evenness", "even"),
    "range"         = c("range"),
    "shannon"       = c("shannon", "shannon index", "shannon entropy"),
    "simpson"       = c("simpson", "simpson index", "simpson diversity"),
    "std"           = c("standard deviation", "std dev", "std"),
    "Contrast"      = c("contrast"),
    "Correlation"   = c("correlation", "corr"),
    "Dissimilarity" = c("dissimilarity"),
    "Entropy"       = c("entropy", "texture entropy"),
    "Homogeneity"   = c("homogeneity"),
    "Maximum"       = c("maximum", "max"),
    "Uniformity"    = c("uniformity", "uniform"),
    "Variance"      = c("variance", "var")
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
  for (canon in names(hetero_lookup)) {
    for (syn in hetero_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    # Also include the canonical code itself normalized
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
      "Unknown EarthEnv habitat heterogeneity variables:",
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
  base_url <- "https://data.earthenv.org/habitat_heterogeneity/1km"
  
  cli::cli_alert_info("Starting the download of EarthEnv habitat heterogeneity data...")
  
  for (canon in requested_codes) {
    # Get the full filename from the lookup list
    filename <- hetero_url_components[[canon]]
    url <- file.path(base_url, filename)
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
    
    
    cli::cli_alert_success("Extraction completed successfully")
    
    # write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    
    return(extracted_df)
  }
}