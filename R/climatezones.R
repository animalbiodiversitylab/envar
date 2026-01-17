# R/climatezones.R

#' Download and process Köppen-Geiger climate classification maps
#'
#' This function downloads, processes, and extracts variables from the
#' High-resolution (1 km) Köppen-Geiger maps dataset. Each variable corresponds
#' to a global GeoTIFF representing climate classification zones based on 
#' historical data or future CMIP6 projections.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item 1 - "zones" ("koppengeiger", "climate", "climatezones", "koppen", "koppen geiger")
#' }
#'
#' \strong{Time Periods} (years argument):
#'
#' \strong{Historical}
#' \itemize{
#'   \item "1901-1930"
#'   \item "1931-1960"
#'   \item "1961-1990"
#'   \item "1991-2020" (default)
#' }
#'
#' \strong{Future}
#' \itemize{
#'   \item "2041-2070"
#'   \item "2071-2099"
#' }
#'
#' \strong{SSP Scenarios} (ssp argument, required for future periods):
#' \itemize{
#'   \item 119 (SSP1-1.9)
#'   \item 126 (SSP1-2.6)
#'   \item 245 (SSP2-4.5)
#'   \item 370 (SSP3-7.0)
#'   \item 434 (SSP4-3.4)
#'   \item 460 (SSP4-6.0)
#'   \item 585 (SSP5-8.5)
#' }
#'
#' \strong{Citation:}\cr
#' Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z, Jiang X, van Dijk AIJM, Miralles DG (2023). "High-resolution (1 km) Köppen-Geiger maps for 1901-2099 based on constrained CMIP6 projections." Scientific Data 10, 724.
#' https://doi.org/10.1038/s41597-023-02549-6
#' 
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector. Defaults to "zones". Accepted aliases include:
#'        "koppengeiger", "climate", "climatezones", "koppen".
#' @param years Character vector of time periods. Defaults to "1991-2020".
#'        Accepts formats with underscores or hyphens (e.g., "1901-1930" or "1901_1930").
#' @param ssp Numeric or character vector of Shared Socioeconomic Pathways.
#'        Required for future projections (e.g., 126, 585).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' climatezones(vars="zones", years="1991-2020")
#'   }
#' @export

climatezones <- function(x, vars = "zones", years = "1991-2020", ssp = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Köppen-Geiger climate classification maps.\n",
    "Citation: Beck HE, McVicar TR, Vergopolan N, Berg A, Lutsko NJ, Dufour A, Zeng Z, Jiang X, van Dijk AIJM, Miralles DG (2023). High-resolution (1 km) Köppen-Geiger maps for 1901-2099 based on constrained CMIP6 projections. Scientific Data 10, 724.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-023-02549-6}\n"
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
  
  # --------------------------------------------------------------------
  # Friendly-name -> validation
  # --------------------------------------------------------------------
  # While vars is mostly a placeholder here, we validate it to ensure 
  # the user intends to download climate zones.
  valid_names <- c("zones", "koppengeiger", "climate", "climatezones", "koppen", "koppen geiger")
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  is_valid_var <- FALSE
  for (v in vars) {
    if (normalize_string(v) %in% valid_names) is_valid_var <- TRUE
  }
  
  if (!is_valid_var) {
    cli::cli_abort(c(
      "Unknown climate variable name requested.",
      "i" = "Please use {.val zones}, {.val koppengeiger}, or {.val climate}."
    ))
  }
  
  # --------------------------------------------------------------------
  # Construct Internal Zip Paths based on Years and SSP
  # --------------------------------------------------------------------
  historical_years <- c("1901_1930", "1931_1960", "1961_1990", "1991_2020")
  future_years     <- c("2041_2070", "2071_2099")
  valid_ssps       <- c("119", "126", "245", "370", "434", "460", "585")
  
  # Normalize input years (replace - with _)
  requested_years <- gsub("-", "_", years)
  
  # Normalize SSPs
  if (!is.null(ssp)) {
    requested_ssp <- as.character(ssp)
    # Ensure they have "ssp" prefix for path construction, strip if user provided "ssp126"
    requested_ssp <- gsub("ssp", "", requested_ssp, ignore.case = TRUE)
    requested_ssp <- paste0("ssp", requested_ssp)
  } else {
    requested_ssp <- NULL
  }
  
  files_to_extract <- list()
  
  for (yr in requested_years) {
    if (yr %in% historical_years) {
      # Historical structure: Folder/file
      internal_path <- file.path(yr, "koppen_geiger_0p01.tif")
      # Create a unique ID for the layer
      layer_id <- paste0("KG_", yr)
      files_to_extract[[layer_id]] <- internal_path
      
    } else if (yr %in% future_years) {
      if (is.null(requested_ssp)) {
        cli::cli_abort("SSP scenarios must be provided for future time period {.val {yr}}.")
      }
      
      for (s in requested_ssp) {
        # Check validity of ssp number
        s_num <- gsub("ssp", "", s)
        if (!s_num %in% valid_ssps) {
          cli::cli_abort("Invalid SSP code: {.val {s_num}}.")
        }
        
        # Future structure: Folder/SSP/file
        internal_path <- file.path(yr, s, "koppen_geiger_0p01.tif")
        # Create a unique ID for the layer
        layer_id <- paste0("KG_", yr, "_", s)
        files_to_extract[[layer_id]] <- internal_path
      }
      
    } else {
      cli::cli_abort("Time period {.val {yr}} is not available in this dataset.")
    }
  }
  
  # --------------------------------------------------------------------
  # Helper: Process and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(dest_file, user_name) {
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        #fs::file_delete(dest_file)
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
      #fs::file_delete(dest_file)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        #fs::file_delete(dest_file)
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
      #fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # Download Logic (One Zip to Rule Them All)
  # --------------------------------------------------------------------
  zip_url <- "https://springernature.figshare.com/ndownloader/files/42602809"
  temp_dir <- fs::path_temp("envar/climate")
  fs::dir_create(temp_dir)
  
  zip_dest <- file.path(temp_dir, "Beck_Koppen_Geiger_v2.zip")
  
  # Check if zip already exists to avoid re-downloading
  if (!file.exists(zip_dest)) {
    cli::cli_alert_info("Downloading global climate zone archive (approx. 3GB)...")
    cli::cli_alert_info("This may take a while, but is only done once.")
    
    success <- download_file_figshare(zip_url, zip_dest)
    if (!success) {
      cli::cli_abort("Failed to download climate data archive from {.url {zip_url}}.")
    }
    cli::cli_alert_success("Archive downloaded successfully.")
  } else {
    cli::cli_alert_info("Using existing climate zone archive found in temporary cache.")
  }
  
  # --------------------------------------------------------------------
  # Extraction and Processing Loop
  # --------------------------------------------------------------------
  cli::cli_alert_info("Extracting and processing requested layers...")
  
  for (layer_id in names(files_to_extract)) {
    internal_path <- files_to_extract[[layer_id]]
    
    cli::cli_alert_info("Unzipping {.val {layer_id}}...")
    
    # Unzip specific file
    tryCatch({
      utils::unzip(zip_dest, files = internal_path, exdir = temp_dir, junkpaths = TRUE)
    }, error = function(e) {
      cli::cli_alert_warning("Could not extract {.val {internal_path}} from zip.")
    })
    
    # The file is now at temp_dir/koppen_geiger_0p01.tif
    # We rename it immediately to avoid overwriting in the next loop iteration
    extracted_filename <- "koppen_geiger_0p01.tif"
    current_file <- file.path(temp_dir, extracted_filename)
    
    if (file.exists(current_file)) {
      # Move to standardized grids directory for extr_check compatibility
      grids_dir <- fs::path_temp("envar/grids")
      fs::dir_create(grids_dir)
      unique_file <- file.path(grids_dir, paste0(layer_id, ".tif"))
      fs::file_move(current_file, unique_file)
      
      # Process
      handle_file(unique_file, layer_id)
    } else {
      cli::cli_alert_warning("File {.val {internal_path}} not found after unzip attempt.")
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
