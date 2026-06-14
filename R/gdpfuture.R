# R/gdpfuture.R

#' Download and process Future GDP Projections (SSP1-5)
#'
#' This function downloads, processes, and extracts variables from the
#' global gridded GDP projections compatible with the five Shared Socioeconomic 
#' Pathways (SSPs) for the period 1850–2100.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#' \itemize{
#'   \item "gdp" ("gross domestic product", "future gdp", "economic projection", "economy", "ssp gdp")
#' }
#'
#' \strong{Scenarios (SSP)}
#' The `ssp` argument accepts integers 1 through 5, corresponding to:
#' \itemize{
#'   \item SSP1: Sustainability
#'   \item SSP2: Middle of the Road
#'   \item SSP3: Regional Rivalry
#'   \item SSP4: Inequality
#'   \item SSP5: Fossil-fueled Development
#' }
#' 
#' \strong{Years available}
#' \itemize{
#'   \item Decadal intervals from 1850 to 2100 (e.g., 2020, 2030, 2040...).
#' }
#'
#' \strong{Citation:}\cr
#' Murakami D, Yoshida T, Yamagata Y (2021). "Gridded GDP projections compatible with the five SSPs (shared socioeconomic pathways)." Frontiers in Built Environment 7, 760306.
#' https://doi.org/10.3389/fbuil.2021.760306
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `par_set()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of variables to download (synonyms for "gdp").
#' @param year Numeric vector of years to download (decades, e.g., 2030, 2050).
#' @param ssp Numeric vector of SSP scenarios to download (values 1 to 5).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Get GDP for SSP1 and SSP5 in 2050
#' processed <- par_set(country= "France", crs=3035) %>% 
#'   gdpfuture(vars="gdp", year=2050, ssp=c(1, 5))
#'
#' # Get time series for SSP2
#' processed <- par_set(country= "India") %>% 
#'   gdpfuture(vars="gdp", year=c(2020, 2030, 2040), ssp=2)
#' }
#' @export

gdpfuture <- function(x, vars, year, ssp, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Future GDP Projections (SSPs).\n",
    "Citation: Murakami D, Yoshida T, Yamagata Y (2021). Gridded GDP projections compatible with the five SSPs (shared socioeconomic pathways). Frontiers in Built Environment 7, 760306.\n",
    "DOI: {.url https://doi.org/10.3389/fbuil.2021.760306}\n"
  ))
  
  # Validate Arguments
  if (missing(year)) cli::cli_abort("Argument {.arg year} is required.")
  if (missing(ssp)) cli::cli_abort("Argument {.arg ssp} (1-5) is required.")
  
  if (any(ssp < 1 | ssp > 5)) {
    cli::cli_abort("SSP values must be integers between 1 and 5.")
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
  gdp_lookup <- list(
    # Metrics
    "gdp" = c("gross domestic product", "future gdp", "economic projection", "economy", "ssp gdp", "gdp")
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
  for (canon in names(gdp_lookup)) {
    for (syn in gdp_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes AND keep mapping to original names
  requested_codes <- character(0)
  code_to_user_name <- list() 
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
      "Unknown variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(big_zip_path, ssp_val, yr, user_name) {
    temp_dir <- fs::path_temp("envar/gdpfuture")
    
    # Define internal path structure based on dataset description
    # Folder: "result(GDP)_SSP{ssp}"
    # File: "gdp{year}.tif"
    inner_folder <- paste0("result(GDP)_SSP", ssp_val)
    tif_name <- paste0("gdp", yr, ".tif")
    path_inside_zip <- file.path(inner_folder, tif_name)
    
    # Construct a descriptive layer name: e.g. "gdp_ssp1_2050"
    final_layer_name <- paste0(user_name, "_ssp", ssp_val, "_", yr)
    
    # Check if the specific year exists in the zip before trying to unzip
    # Listing files in a huge zip can be slow, so we just try to unzip and catch error
    
    temp_dest_file <- file.path(temp_dir, tif_name)
    
    # Unzip only the specific file
    cli::cli_alert_info("Extracting {.val {tif_name}} (SSP{.val {ssp_val}})...")
    
    unzip_success <- try(utils::unzip(big_zip_path, files = path_inside_zip, exdir = temp_dir, junkpaths = TRUE, overwrite = TRUE), silent=TRUE)
    
    if (inherits(unzip_success, "try-error") || !file.exists(temp_dest_file)) {
      cli::cli_alert_warning("Could not find {.val {tif_name}} in SSP{.val {ssp_val}} folder. Check if year {.val {yr}} is available.")
      return(NULL)
    }
    
    # Copy to standardized path for extr_check compatibility
    grids_dir <- envar_grids_dir()
    fs::dir_create(grids_dir)
    dest_file <- file.path(grids_dir, paste0(final_layer_name, ".tif"))
    fs::file_copy(temp_dest_file, dest_file, overwrite = TRUE)
    #fs::file_delete(temp_dest_file)
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {final_layer_name}}...")
      
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
      
      # Assign constructed name to layer
      names(layer1) <- final_layer_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {final_layer_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      #fs::file_delete(dest_file)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {final_layer_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {final_layer_name}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
        names(extracted)[ncol(extracted)] <- final_layer_name
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {final_layer_name}} successfully.")
      
      rm(extracted)
      gc()
     # fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables, SSPs, and Years
  # --------------------------------------------------------------------
  
  # Download the main archive once
  url <- "https://figshare.com/ndownloader/files/22078776"
  temp_base <- fs::path_temp("envar/gdpfuture")
  fs::dir_create(temp_base)
  big_zip_path <- file.path(temp_base, "GDP_SSP_grid.zip")
  
  cli::cli_alert_info("Processing Future GDP data...")
  
  if (!file.exists(big_zip_path)) {
    cli::cli_alert_info("Downloading main archive (this may take a moment)...")
    success <- download_file_figshare(url, big_zip_path)
    if (!success) {
      cli::cli_abort("Failed to download main archive from {.url {url}}.")
    }
  } else {
    cli::cli_alert_info("Using cached archive.")
  }
  
  # Triple loop: Vars (synonyms) -> SSPs -> Years
  for (canon in requested_codes) {
    user_name <- code_to_user_name[[canon]]
    
    for (s in ssp) {
      for (y in year) {
        handle_file(big_zip_path, s, y, user_name)
      }
    }
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
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path
    
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
