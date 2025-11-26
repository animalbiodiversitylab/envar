# R/climatezones.R

#' Download and process Köppen-Geiger climate classification maps
#'
#' `climatezones()` downloads, processes, and extracts variables from the
#' **High-resolution (1 km) Köppen-Geiger maps** dataset.
#' Each variable corresponds to a global GeoTIFF representing climate classification
#' zones based on historical data or future CMIP6 projections.
#'
#' The function automatically:
#' - Checks for the dataset zip file (downloading it once if necessary),
#' - Extracts the specific time periods and SSP scenarios requested,
#' - Crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - Extracts values when `x` is point data.
#'
#' This function specifically targets the **0.01 degree resolution** layers
#' (approximately 1km).
#'
#' ## Citation
#' If you use this data, please cite:
#'
#' **Beck, H.E., T.R. McVicar, N. Vergopolan, A. Berg, N.J. Lutsko, A. Dufour, Z. Zeng, X. Jiang, A.I.J.M. van Dijk, D.G. Miralles (2023).**
#' *High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6 projections.*
#' Scientific Data **10**, 724.
#' https://doi.org/10.1038/s41597-023-02549-6
#'
#'
#' ## Available Arguments
#'
#' The `vars` argument is provided for consistency but defaults to `"zones"`.
#' Users should control the output primarily via the `years` and `ssp` arguments.
#'
#' ### Time Periods (`years`)
#' **Historical:**
#' * `"1901-1930"`
#' * `"1931-1960"`
#' * `"1961-1990"`
#' * `"1991-2020"`
#'
#' **Future:**
#' * `"2041-2070"`
#' * `"2071-2099"`
#'
#' ### SSP Scenarios (`ssp`)
#' Required if a future time period is selected.
#' * `119` (SSP1-1.9)
#' * `126` (SSP1-2.6)
#' * `245` (SSP2-4.5)
#' * `370` (SSP3-7.0)
#' * `434` (SSP4-3.4)
#' * `460` (SSP4-6.0)
#' * `585` (SSP5-8.5)
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector. Defaults to `"zones"`. Accepted aliases include:
#'             `"koppengeiger"`, `"climate"`, `"climatezones"`, `"koppen"`.
#' @param years Character vector of time periods. Defaults to `"1991-2020"`.
#'              Accepts formats with underscores or hyphens (e.g., `"1901-1930"` or `"1901_1930"`).
#' @param ssp Numeric or character vector of Shared Socioeconomic Pathways.
#'            Required for future projections (e.g., `126`, `585`).
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed climate zone layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

climatezones <- function(x, vars = "zones", years = "1991-2020", ssp = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Köppen-Geiger climate classification maps.\n",
    "Citation: Beck, H.E., et al. (2023). High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6 projec­tions. Scientific Data.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-023-02549-6}"
  ))
  
  par_list <- get_par(x)
  
  if (inherits(par_list[[1]], "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res  <- par_list$res
    crs  <- par_list$crs
    is_raster_input <- TRUE
  } else {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    is_raster_input <- FALSE
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> validation
  # --------------------------------------------------------------------
  # While vars is mostly a placeholder here, we validate it to ensure 
  # the user intends to download climate zones.
  valid_names <- c("zones", "koppengeiger", "climate", "climatezones", "koppen", "koppen geiger")
  
  # Normalizer
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
  handle_file <- function(dest_file, var_name) {
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {var_name}}...")
      
      layer <- terra::crop(layer, grid, snap = "out")
      layer <- terra::resample(layer, grid, method = "near") # Categorical data uses 'near'
      layer <- terra::mask(layer, mask)
      
      # Rename the layer inside the raster object
      names(layer) <- var_name
      
      if (!is.null(par_list$crs)) {
        layer <- terra::project(layer, par_list$crs, method = "near")
      }
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer
      } else {
        processed_stack <<- c(processed_stack, layer)
      }
      
      cli::cli_alert_success("Processed and added {.val {var_name}} to stack.")
      
      rm(layer)
      gc()
      fs::file_delete(dest_file)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {var_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {var_name}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      # Rename value column to the specific variable name
      colnames(extracted)[which(names(extracted) != "ID")] <- var_name
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted, by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {var_name}} successfully.")
      
      rm(extracted)
      gc()
      fs::file_delete(dest_file)
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
    
    success <- download_file(zip_url, zip_dest)
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
    
    # We extract the specific file to the temp dir
    # Note: unzip behavior varies by OS, but 'files' argument helps limit extraction
    # We flatten the path on extraction to make it easier to find
    
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
      unique_file <- file.path(temp_dir, paste0(layer_id, ".tif"))
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
    if (inherits(x, "SpatRaster")) processed_stack <- c(x, processed_stack)
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}