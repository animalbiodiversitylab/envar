# R/aridity.R

#' Download and process Global Aridity Index and Potential Evapotranspiration (ET0)
#'
#' @export
aridity <- function(x, vars, ...) {
  
  old_timeout <- getOption("timeout")
  options(timeout=max(100000000000000,old_timeout))
  on.exit(options(timeout=old_timeout))
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Aridity Index and ET0 Database v3.\n",
    "Citation: Zomer, R. J., Xu, J., & Trabucco, A. (2022). Version 3 of the Global Aridity Index and Potential Evapotranspiration Database. Scientific Data.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-022-01493-1}\n",
    "Note: Data is downloaded from Figshare (Article ID 7504448)."
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
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  
  # Define which sub-zip folder each variable belongs to
  zip_map <- list(
    "Global-ET0_monthly_v3"      = paste0("et0_v3_", sprintf("%02d", 1:12), ".tif"),
    "Global-AI_ET0_annual_v3"    = c("et0_v3_yr_sd.tif", "et0_v3_yr.tif", "ai_v3_yr.tif"),
    "Global-AI_monthly_v3"       = paste0("ai_v3_", sprintf("%02d", 1:12), ".tif")
  )
  
  # Reverse map: filename -> sub-zip name
  file_to_zip <- character()
  for (z in names(zip_map)) {
    for (f in zip_map[[z]]) {
      file_to_zip[f] <- z
    }
  }
  
  # Build Friendly Name Dictionary
  aridity_lookup <- list()
  
  # Annual Variables
  aridity_lookup[["ai_v3_yr.tif"]]      <- c("aridity index annual", "ai annual", "aridity annual", "ai year")
  aridity_lookup[["et0_v3_yr.tif"]]     <- c("et0 annual", "potential evapotranspiration annual", "evapotranspiration annual", "et0 year")
  aridity_lookup[["et0_v3_yr_sd.tif"]] <- c("et0 standard deviation", "et0 sd", "et0 variability", "et0 annual sd")
  
  # Monthly AI Variables (Jan-Dec)
  for (i in 1:12) {
    code <- sprintf("ai_v3_%02d.tif", i)
    m_name <- tolower(month.name[i])
    m_abb <- tolower(month.abb[i])
    aridity_lookup[[code]] <- c(
      paste("aridity index", m_name), paste("ai", m_name), 
      paste("aridity index", m_abb),  paste("ai", m_abb),
      paste("ai", i), paste("aridity", i)
    )
  }
  
  # Monthly ET0 Variables (Jan-Dec)
  for (i in 1:12) {
    code <- sprintf("et0_v3_%02d.tif", i)
    m_name <- tolower(month.name[i])
    m_abb <- tolower(month.abb[i])
    aridity_lookup[[code]] <- c(
      paste("et0", m_name), paste("potential evapotranspiration", m_name),
      paste("et0", m_abb),
      paste("et0", i)
    )
  }
  
  # Normalizer
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(aridity_lookup)) {
    for (syn in aridity_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes
  requested_codes <- character(0)
  unmapped <- character(0)
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      requested_codes <- c(requested_codes, syn2canon[[key]])
    } else {
      unmapped <- c(unmapped, v)
    }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c(
      "Unknown Aridity/ET0 variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download (unzip), process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var) {
    
    temp_dir <- fs::path_temp("envar/aridity")
    fs::dir_create(temp_dir)
    
    # 1. Identify necessary zips based on the requested variable
    sub_zip_name <- file_to_zip[var]
    main_zip_file <- file.path(temp_dir, "Global_Aridity_ET0_v3.zip")
    sub_zip_file  <- file.path(temp_dir, paste0(sub_zip_name, ".zip"))
    
    # NOTE: final_tif path is dynamic now, we determine it after inspecting the zip
    # But we can assume if the extraction logic works, we will find the file.
    
    # Check if sub-zip exists, if not, we need to get it from the main zip
    if (!fs::file_exists(sub_zip_file)) {
      
      # Check if main zip exists, if not, download it
      if (!fs::file_exists(main_zip_file)) {
        cli::cli_alert_info("Downloading main archive (approx 1.5 GB)... This happens only once.")
        success <- download_file(url, main_zip_file)
        if (!success) {
          cli::cli_alert_warning("Failed to download main archive from {.url {url}}.")
          return(NULL)
        }
      }
      
      cli::cli_alert_info("Unzipping {.val {sub_zip_name}} from main archive...")
      tryCatch({
        utils::unzip(main_zip_file, files = paste0(sub_zip_name, ".zip"), exdir = temp_dir)
      }, error = function(e) {
        cli::cli_alert_warning("Failed to unzip main archive.")
        return(NULL)
      })
    }
    
    # --- Inspect sub-zip content to find the actual file path ---
    cli::cli_alert_info("Locating {.val {var}} inside {.val {sub_zip_name}}...")
    
    # List files inside the sub-zip
    zip_contents <- try(utils::unzip(sub_zip_file, list = TRUE), silent = TRUE)
    
    if (inherits(zip_contents, "try-error")) {
      cli::cli_alert_warning("Could not list contents of {.val {sub_zip_file}}.")
      return(NULL)
    }
    
    # Search for the file in the zip list (matches "folder/file.tif" or "file.tif")
    # We look for a file that ends with our requested variable name
    target_file_in_zip <- grep(paste0(var, "$"), zip_contents$Name, value = TRUE)
    
    if (length(target_file_in_zip) == 0) {
      cli::cli_alert_warning("File {.val {var}} not found inside the sub-zip archive.")
      return(NULL)
    }
    
    # Use the first match (usually there's only one)
    target_file_in_zip <- target_file_in_zip[1]
    final_tif <- file.path(temp_dir, target_file_in_zip)
    
    # Only extract if not already extracted
    if (!fs::file_exists(final_tif)) {
      cli::cli_alert_info("Extracting {.val {var}}...")
      tryCatch({
        utils::unzip(sub_zip_file, files = target_file_in_zip, exdir = temp_dir)
      }, error = function(e) {
        cli::cli_alert_warning("Failed to extract inner file.")
        return(NULL)
      })
    }

    # 3. Process the extracted TIF
    if (is_raster_input) {
      layer <- try(terra::rast(final_tif), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {final_tif}}.")
        # Only delete if it exists to avoid error
        if(fs::file_exists(final_tif)) fs::file_delete(final_tif)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {var}}...")
      
      layer <- terra::crop(layer, grid, snap = "out")
      layer <- terra::resample(layer, grid, method = "bilinear")
      layer <- terra::mask(layer, mask)
      
      if (!is.null(par_list$crs)) {
        layer <- terra::project(layer, par_list$crs)
      }
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer
      } else {
        processed_stack <<- c(processed_stack, layer)
      }
      
      cli::cli_alert_success("Processed and added {.val {var}} to stack.")
      
      rm(layer)
      gc()
      # Delete the unzipped TIF to save space
      if(fs::file_exists(final_tif)) fs::file_delete(final_tif)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {var}}...")
      
      extracted <- try(process_points(file = final_tif, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {var}}.")
        if(fs::file_exists(final_tif)) fs::file_delete(final_tif)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, 4)], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {var}} successfully.")
      
      rm(extracted)
      gc()
      if(fs::file_exists(final_tif)) fs::file_delete(final_tif)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  base_url <- "https://figshare.com/ndownloader/articles/7504448/versions/5"
  
  cli::cli_alert_info("Starting the download/extraction of Global Aridity Index data...")
  
  for (canon in requested_codes) {
    handle_file(base_url, canon, canon)
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