# R/topography.R

#' Download and process EarthEnv Topography layers
#'
#' This function downloads, processes, and extracts variables from the
#' EarthEnv Topography dataset. This dataset provides global, cross-scale
#' topographic variables suitable for biodiversity and ecosystem modeling.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item 1 - "elevation" ("dem", "height", "alt", "altitude")
#'   \item 2 - "slope" 
#'   \item 3 - "aspect"
#'   \item 4 - "roughness" ("rough")
#'   \item 5 - "tri" ("terrain ruggedness index", "ruggedness")
#'   \item 6 - "tpi" ("topographic position index", "position")
#'   \item 7 - "vrm" ("vector ruggedness measure")
#'   \item 8 - "pcurv" ("profile curvature", "profile curve")
#'   \item 9 - "tcurv" ("tangential curvature", "tangential curve")
#'   \item 10 - "eastness" ("east")
#'   \item 11 - "northness" ("north")
#' }
#'
#' \strong{Citation:}\cr
#' Amatulli, G., Domisch, S., Tuanmu, M.-N., Parmentier, B., Ranipeta, A.,
#' Malczyk, J., and Jetz, W. (2018). "A suite of global, cross-scale topographic 
#' variables for environmental and biodiversity modeling." Scientific Data 5: 180040.
#' https://doi.org/10.1038/sdata.2018.40
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param algorithm Character. The aggregation method/algorithm to use. 
#' \itemize{
#'   \item Common options: "md" (median, default), "mn" (mean), "min", "max", "sd".
#'   \item Note: These codes directly affect the downloaded filename (e.g., \code{_1KMmd_}).
#' }
#' @param topo_source Character. The source of the data. 
#' \itemize{
#'   \item "GMTED" (Global Multi-resolution Terrain Elevation Data) - Default.
#'   \item "SRTM" (Shuttle Radar Topography Mission).
#' }
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country = "Italy", crs = 3035) %>% 
#' topography(vars = c("elevation", "slope"))
#'   }
#' @export

topography <- function(x, vars, algorithm = "md", topo_source = "GMTED", ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Topography layers.\n",
    "Citation: Amatulli, G., et al. (2018). A suite of global, cross-scale topographic variables for environmental and biodiversity modeling. Scientific Data.\n",
    "DOI: {.url https://doi.org/10.1038/sdata.2018.40}\n"
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
    set_na=par_list$set_na
    path = par_list$path
    land= par_list$land
    # Track cumulative global extent
    current_global_extent <- par_list$global_extent
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs <- par_list$crs
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
  # Validate additional arguments
  # --------------------------------------------------------------------
  source_upper <- toupper(topo_source)
  valid_sources <- c("GMTED", "SRTM")
  
  if (!source_upper %in% valid_sources) {
    cli::cli_abort("The parameter {.arg topo_source} must be one of: {.val {valid_sources}}")
  }
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  topo_lookup <- list(
    "elevation" = c("elevation", "dem", "height", "alt", "altitude"),
    "slope"     = c("slope"),
    "aspect"    = c("aspect"),
    "roughness" = c("roughness", "rough"),
    "tri"       = c("tri", "terrain ruggedness index", "ruggedness"),
    "tpi"       = c("tpi", "topographic position index", "position"),
    "vrm"       = c("vrm", "vector ruggedness measure"),
    "pcurv"     = c("pcurv", "profile curvature", "profile curve"),
    "tcurv"     = c("tcurv", "tangential curvature", "tangential curve"),
    "eastness"  = c("eastness", "east"),
    "northness" = c("northness", "north")
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
  for (canon in names(topo_lookup)) {
    for (syn in topo_lookup[[canon]]) {
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
      "Unknown Topography variables:",
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
  base_url <- "https://data.earthenv.org/topography"
  
  cli::cli_alert_info("Starting the download of EarthEnv Topography data...")
  
  for (canon in requested_codes) {
    
    # Apply logic to determine filename parts
    current_alg <- algorithm
    
    # Special case for elevation max algorithm
    if (canon == "elevation" && algorithm == "max") {
      current_alg <- "ma"
    }
    
    # Logic for source part of filename
    source_filename_part <- if (source_upper == "GMTED") {
      paste0(source_upper, current_alg)
    } else {
      source_upper
    }
    
    # Construct filename: {var}_1KM{alg}_{source_part}.tif
    file_name <- paste0(canon, "_1KM", current_alg, "_", source_filename_part, ".tif")
    
    url <- file.path(base_url, file_name)
    dest <- file.path(fs::path_temp("envar/grids"), file_name)
    
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