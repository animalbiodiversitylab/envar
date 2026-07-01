# R/accessibility.R

#' Download and process Global Accessibility Indicators
#'
#' This function downloads, processes, and extracts variables from the
#' Global Accessibility Indicators dataset. Each variable corresponds to a 
#' global raster representing the travelling time (in minutes) to cities or 
#' ports of specific sizes.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Cities}
#' \itemize{
#'   \item 1 - "cities1" ("cities 1", "city 1", "cities >5m", "huge cities", "travel time cities 1")
#'   \item 2 - "cities2" ("cities 2", "city 2", "cities >1m", "large cities", "travel time cities 2")
#'   \item 3 - "cities3" ("cities 3", "city 3", "medium cities", "travel time cities 3")
#'   \item 4 - "cities4" ("cities 4", "city 4", "small cities", "travel time cities 4")
#'   \item 5 - "cities5" ("cities 5", "city 5", "travel time cities 5")
#'   \item 6 - "cities6" ("cities 6", "city 6", "travel time cities 6")
#'   \item 7 - "cities7" ("cities 7", "city 7", "travel time cities 7")
#'   \item 8 - "cities8" ("cities 8", "city 8", "towns", "travel time cities 8")
#'   \item 9 - "cities9" ("cities 9", "city 9", "small towns", "travel time cities 9")
#'   \item 10 - "cities10" ("cities 10", "city 10", "aggregated cities 1", "travel time cities 10")
#'   \item 11 - "cities11" ("cities 11", "city 11", "aggregated cities 2", "travel time cities 11")
#'   \item 12 - "cities12" ("cities 12", "city 12", "aggregated cities 3", "travel time cities 12")
#' }
#'
#' \strong{Ports}
#' \itemize{
#'   \item 13 - "ports1" ("ports 1", "port 1", "large ports", "travel time ports 1")
#'   \item 14 - "ports2" ("ports 2", "port 2", "medium ports", "travel time ports 2")
#'   \item 15 - "ports3" ("ports 3", "port 3", "small ports", "travel time ports 3")
#'   \item 16 - "ports4" ("ports 4", "port 4", "very small ports", "travel time ports 4")
#'   \item 17 - "ports5" ("ports 5", "port 5", "any port", "all ports", "travel time ports 5")
#' }
#'
#' \strong{Citation:}\cr
#' Nelson A, Weiss DJ, van Etten J et al (2019). "A suite of global accessibility indicators." Scientific Data 6, 266.
#' https://doi.org/10.1038/s41597-019-0265-5
#'
#' Note: Data extent is [-180, 180, -60, 85].
#' 
#' @param x The output from `par_set()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- par_set(country = "Italy", crs = 3035) %>% 
#' accessibility(vars=c("large cities", "ports1"))
#'   }
#' @export

accessibility <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Accessibility Indicators.\n",
    "Citation: Nelson A, Weiss DJ, van Etten J et al (2019). A suite of global accessibility indicators. Scientific Data 6, 266.\n",
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
    set_na=par_list$set_na
    path = par_list$path
    land = par_list$land
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
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
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
  
  # Direct URL lookup (Figshare)
  url_lookup <- list(
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
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
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
      "Unknown Accessibility variables:",
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
    
    success <- download_file_figshare(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
         # fs::file_delete(dest_file)
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
        #fs::file_delete(dest_file)
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
        #fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of Accessibility data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- url_lookup[[canon]]
    
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
    # Write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
