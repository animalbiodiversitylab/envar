# R/pftlandcover.R

#' Download and process Global PFT Land Cover Projections (SSPs-RCPs)
#'
#' This function downloads, processes, and extracts land cover variables from the
#' Global 7-land-types LULC projection dataset based on plant functional types (PFT)
#' with a 1-km resolution under socio-climatic scenarios (Chen et al., 2022).
#'
#' Available variables (working synonyms in parentheses):
#'
#' 1 - "landcover" ("cover", "land", "lulc", "pft")
#'
#' Note: If the `vars` argument is left empty, the function will default 
#' to downloading the land cover map.
#'
#' Required Arguments:
#'
#' `year`: Integer. Available years: 2020 to 2100 in 5-year intervals 
#' (2020, 2025, 2030, ..., 2100).
#'
#' `ssp`: Integer or Character. The SSP-RCP scenario code. 
#' Available values: 119, 126, 245, 370, 434, 460, 534, 585.
#' (e.g., 585 corresponds to SSP5-RCP8.5).
#'
#' Citation:
#'
#' Chen, G., Li, X. & Liu, X. (2022). "Global land projection based on plant functional types 
#' with a 1-km resolution under socio-climatic scenarios." 
#' Sci Data 9, 125.
#' https://doi.org/10.1038/s41597-022-01209-6
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of variables to download. Defaults to "landcover" if empty.
#' @param year Integer. The year of the projection (2020-2100, step 5). Defaults to 2025.
#' @param ssp Integer or Character. The SSP-RCP scenario code (119, 126, 245, 370, 434, 460, 534, 585). Defaults to 585.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Download SSP5-RCP8.5 projection for 2050
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#'   pftlandcover(vars="landcover", year=2050, ssp=585)
#'   }
#' @export

pftlandcover <- function(x, vars = NULL, year = 2025, ssp = 585, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global PFT Land Cover Projections (Chen et al., 2022).\n",
    "Citation: Sci Data 9, 125 (2022).\n",
    "DOI: {.url https://doi.org/10.1038/s41597-022-01209-6}\n"
  ))
  
  # --------------------------------------------------------------------
  # Argument Validation
  # --------------------------------------------------------------------
  
  # Validate Year
  valid_years <- seq(2020, 2100, by = 5)
  if (!year %in% valid_years) {
    cli::cli_abort(c(
      "Invalid year: {.val {year}}.",
      "i" = "Available years: 2020 to 2100 (5-year intervals)."
    ))
  }
  
  # Validate SSP
  valid_ssps <- c(119, 126, 245, 370, 434, 460, 534, 585)
  if (!ssp %in% valid_ssps) {
    cli::cli_abort(c(
      "Invalid SSP code: {.val {ssp}}.",
      "i" = "Available SSPs: {.val {valid_ssps}}."
    ))
  }
  
  # Parse SSP into component parts for filename construction
  # The first digit is the SSP scenario (1-5), the rest is the RCP (e.g. 19, 26, 85)
  ssp_str <- as.character(ssp)
  ssp_num <- substr(ssp_str, 1, 1) # First char
  rcp_num <- substr(ssp_str, 2, nchar(ssp_str)) # Remaining chars
  
  # --------------------------------------------------------------------
  # Standard Setup
  # --------------------------------------------------------------------
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
  land_lookup <- list(
    "landcover" = c("landcover", "cover", "land", "lulc", "pft", "projection")
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
      "Unknown PFT Land Cover variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name, internal_file) {
    
    # Check if the specific TIF exists; if not, check/download zip and extract
    if (!file.exists(dest_file)) {
      temp_dir <- dirname(dest_file)
      # The main zip file path
      zip_dest <- file.path(temp_dir, "Global_7-land-types_LULC.zip")
      
      # 1. Ensure Zip exists (it's big, so we check carefully)
      if (!file.exists(zip_dest)) {
        cli::cli_alert_info("Downloading Global LULC Zip archive (approx 180MB)...")
        # Note: The provided URL might need proper quoting if passed to system commands,
        # but download.file usually handles it.
        success <- download_file(url, zip_dest)
        if (!success) {
          cli::cli_alert_warning("Failed to download Zip from {.url {url}}.")
          return(NULL)
        }
      } else {
        cli::cli_alert_info("Using existing Zip archive.")
      }
      
      # 2. Extract specific file
      cli::cli_alert_info("Extracting {.val {internal_file}} from archive...")
      
      # Determine internal path inside the zip
      # We will list files first to find the match if unsure, or try direct extraction.
      
      # Attempt extraction
      # We use unzip's list to find the full internal path in case of subfolders
      
      file_list <- utils::unzip(zip_dest, list = TRUE)
      
      # Find the row that ends with our target filename
      match_idx <- grep(paste0(internal_file, "$"), file_list$Name)
      
      if (length(match_idx) == 0) {
        cli::cli_alert_warning("Could not find {.val {internal_file}} inside the zip archive.")
        return(NULL)
      }
      
      full_internal_path <- file_list$Name[match_idx[1]]
      
      extract_result <- try(utils::unzip(zip_dest, files = full_internal_path, exdir = temp_dir, junkpaths = TRUE), silent = TRUE)
      
      if (inherits(extract_result, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {internal_file}}.")
        return(NULL)
      }
    }
    
    # After extraction with junkpaths=TRUE, the file should be at dest_file (if dest_file is just filename in temp)
    # Ensure dest_file points to the extracted TIF location
    
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
      # Append ssp and year to name for clarity
      names(layer1) <- paste0(user_name, "_SSP", ssp, "_", year)
      
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
        # Use user-requested name for the column (append ssp/year)
        names(extracted)[ncol(extracted)] <- paste0(user_name, "_SSP", ssp, "_", year)
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
  
  # Create temp dir for this batch
  temp_dir <- fs::path_temp("envar/pftlandcover")
  fs::dir_create(temp_dir)
  
  cli::cli_alert_info("Processing PFT Land Cover data (SSP{.val {ssp}}, Year {.val {year}})...")
  
  # Main ZIP URL
  zip_url <- "https://zenodo.org/records/4584775/files/Global%207-land-types%20LULC%20projection%20dataset%20under%20SSPs-RCPs.zip?download=1"
  
  for (canon in requested_codes) {
    # Construct filename based on SSP and Year
    # Format from instructions: global_SSP5_RCP85_2025.tif
    # We derived ssp_num and rcp_num earlier
    # Example: ssp=585 -> ssp_num=5, rcp_num=85 -> global_SSP5_RCP85_2025.tif
    
    internal_filename <- paste0("global_SSP", ssp_num, "_RCP", rcp_num, "_", year, ".tif")
    
    # Destination for the extracted TIF
    dest <- file.path(temp_dir, internal_filename)
    
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    
    # Pass the ZIP url, but dest is the TIF
    handle_file(zip_url, dest, canon, user_name, internal_filename)
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