# R/aridity.R

#' Download and process Global Aridity Index and Potential Evapotranspiration (ET0)
#'
#' This function downloads, processes, and extracts variables from the
#' Global Aridity Index and ET0 Database v3. Each variable corresponds to a global
#' raster representing aridity index or potential evapotranspiration values.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Annual Variables}
#' \itemize{
#'   \item "ai_v3_yr.tif" ("aridity index annual", "ai annual", "aridity annual", "ai year")
#'   \item "et0_v3_yr.tif" ("et0 annual", "potential evapotranspiration annual", "et0 year")
#'   \item "et0_v3_yr_sd.tif" ("et0 standard deviation", "et0 sd", "et0 variability")
#' }
#'
#' \strong{Monthly Aridity Index}
#' \itemize{
#'   \item "ai_v3_01.tif" ... "ai_v3_12.tif" ("aridity index january"..."december", "ai jan"..."dec", "ai 01"..."12")
#' }
#'
#' \strong{Monthly Potential Evapotranspiration (ET0)}
#' \itemize{
#'   \item "et0_v3_01.tif" ... "et0_v3_12.tif" ("et0 january"..."december", "et0 jan"..."dec", "et0 01"..."12")
#' }
#'
#' \strong{Citation:}\cr
#' Zomer RJ, Xu J, Trabucco A (2022). "Version 3 of the Global Aridity Index and Potential Evapotranspiration Database." Scientific Data 9, 409.
#' https://doi.org/10.1038/s41597-022-01493-1
#'
#' Note: Data is downloaded from Figshare (Article ID 7504448).
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
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' aridity(vars=c("aridity index annual", "et0 january"))
#'   }
#' @export

aridity <- function(x, vars, ...) {
  
  old_timeout <- getOption("timeout")
  options(timeout = max(100000000000000, old_timeout))
  on.exit(options(timeout = old_timeout))
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Aridity Index and ET0 Database v3.\n",
    "Citation: Zomer RJ, Xu J, Trabucco A (2022). Version 3 of the Global Aridity Index and Potential Evapotranspiration Database. Scientific Data 9, 409.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-022-01493-1}\n"
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
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  
  # Define which sub-zip folder each variable belongs to
  zip_map <- list(
    "Global-ET0_monthly_v3"   = paste0("et0_v3_", sprintf("%02d", 1:12), ".tif"),
    "Global-AI_ET0_annual_v3" = c("et0_v3_yr_sd.tif", "et0_v3_yr.tif", "ai_v3_yr.tif"),
    "Global-AI_monthly_v3"    = paste0("ai_v3_", sprintf("%02d", 1:12), ".tif")
  )
  
  # Reverse map: filename -> sub-zip name
  file_to_zip <- character()
  for (z in names(zip_map)) {
    for (f in zip_map[[z]]) {
      file_to_zip[f] <- z
    }
  }
  
  # Build friendly name dictionary
  aridity_lookup <- list()
  
  # Annual variables
  aridity_lookup[["ai_v3_yr.tif"]]     <- c("aridity index annual", "ai annual", "aridity annual", "ai year")
  aridity_lookup[["et0_v3_yr.tif"]]    <- c("et0 annual", "potential evapotranspiration annual", "evapotranspiration annual", "et0 year")
  aridity_lookup[["et0_v3_yr_sd.tif"]] <- c("et0 standard deviation", "et0 sd", "et0 variability", "et0 annual sd")
  
  # Monthly aridity index variables (Jan-Dec)
  for (i in 1:12) {
    code <- sprintf("ai_v3_%02d.tif", i)
    m_name <- tolower(month.name[i])
    m_abb <- tolower(month.abb[i])
    aridity_lookup[[code]] <- c(
      paste("aridity index", m_name), paste("ai", m_name), 
      paste("aridity index", m_abb), paste("ai", m_abb),
      paste("ai", i), paste("aridity", i)
    )
  }
  
  # Monthly potential evapotranspiration variables (Jan-Dec)
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
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
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
  
  # Convert requested vars to canonical codes and keep mapping to original names
  requested_codes <- character(0)
  code_to_user_name <- list() # Maps canonical code -> user's original name
  unmapped <- character(0)
  
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      canon <- syn2canon[[key]]
      # Only add if not already present
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
      "Unknown Aridity/ET0 variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download (unzip), process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, canon, user_name) {
    
    temp_dir <- fs::path_temp("envar/aridity")
    fs::dir_create(temp_dir)
    
    # Identify necessary zips based on the requested variable
    sub_zip_name <- file_to_zip[canon]
    main_zip_file <- file.path(temp_dir, "Global_Aridity_ET0_v3.zip")
    sub_zip_file <- file.path(temp_dir, paste0(sub_zip_name, ".zip"))
    
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
    
    # Inspect sub-zip content to find the actual file path
    cli::cli_alert_info("Locating {.val {user_name}} inside {.val {sub_zip_name}}...")
    
    # List files inside the sub-zip
    zip_contents <- try(utils::unzip(sub_zip_file, list = TRUE), silent = TRUE)
    
    if (inherits(zip_contents, "try-error")) {
      cli::cli_alert_warning("Could not list contents of {.val {sub_zip_file}}.")
      return(NULL)
    }
    
    # Search for the file in the zip list
    target_file_in_zip <- grep(paste0(canon, "$"), zip_contents$Name, value = TRUE)
    
    if (length(target_file_in_zip) == 0) {
      cli::cli_alert_warning("File {.val {canon}} not found inside the sub-zip archive.")
      return(NULL)
    }
    
    target_file_in_zip <- target_file_in_zip[1]
    final_tif <- file.path(temp_dir, target_file_in_zip)
    
    if (!fs::file_exists(final_tif)) {
      cli::cli_alert_info("Extracting {.val {user_name}}...")
      tryCatch({
        utils::unzip(sub_zip_file, files = target_file_in_zip, exdir = temp_dir)
      }, error = function(e) {
        cli::cli_alert_warning("Failed to extract inner file.")
        return(NULL)
      })
    }
    
    # Process the extracted TIF
    if (is_raster_input) {
      layer <- try(terra::rast(final_tif), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {final_tif}}.")
        if (fs::file_exists(final_tif)) fs::file_delete(final_tif)
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
      # Delete the unzipped TIF to save space
      if (fs::file_exists(final_tif)) fs::file_delete(final_tif)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = final_tif, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (fs::file_exists(final_tif)) fs::file_delete(final_tif)
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
      if (fs::file_exists(final_tif)) fs::file_delete(final_tif)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  base_url <- "https://figshare.com/ndownloader/articles/7504448/versions/5"
  
  cli::cli_alert_info("Starting the download/extraction of Global Aridity Index data...")
  
  for (canon in requested_codes) {
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    
    handle_file(base_url, canon, user_name)
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
    
    cli::cli_alert_success("Extraction completed successfully")
    
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    
    return(extracted_df)
  }
}