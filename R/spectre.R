# R/spectre.R

#' Download and process SPECTRE environmental threat layers
#'
#' This function downloads, processes, and extracts variables from the
#' SPECTRE — Spatially Explicit ECosysTem ThREats dataset. Each variable 
#' corresponds to a global Cloud-Optimized GeoTIFF (COG) representing a 
#' different anthropogenic or climatic threat.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Land Use and Human Pressure}
#' \itemize{
#'   \item 1 - "1_1_MINING_AREA_cog" ("mining area", "mining_area", "mining")
#'   \item 2 - "1_2_HAZARD_POTENTIAL_cog" ("hazard potential", "hazard")
#'   \item 3 - "1_3_HUMAN_DENSITY_cog" ("human density", "population", "pop")
#'   \item 4 - "1_4_BUILT_AREA_cog" ("built area", "built")
#'   \item 5 - "1_5_ROAD_DENSITY_cog" ("road density", "roads", "road")
#'   \item 6 - "1_6_FOOTPRINT_PERC_cog" ("human footprint", "footprint")
#'   \item 7 - "1_7_IMPACT_AREA_cog" ("impacted area", "impact area")
#'   \item 8 - "1_8_MODIF_AREA_cog" ("modified area", "modif area")
#'   \item 9 - "1_9_HUMAN_BIOMES_cog" ("human biomes", "biomes")
#'   \item 10 - "1_10_FIRE_OCCUR_cog" ("fires", "fire")
#'   \item 11 - "1_11_CROP_PERC_UNI_cog" ("crops uni", "crop uni", "crop")
#'   \item 12 - "1_12_CROP_PERC_IIASA_cog" ("crops iiasa", "iiasa crops")
#'   \item 13 - "1_13_LIVESTOCK_MASS_cog" ("livestock", "livestock mass")
#' }
#' 
#' \strong{Forest Loss}
#' \itemize{
#'   \item 14 - "2_1_FOREST_LOSS_PERC_cog" ("forest loss")
#'   \item 15 - "2_2_FOREST_TREND_cog" ("forest trend")
#' }
#' 
#' \strong{Light Pollution}
#' \itemize{
#'   \item 16 - "3_1_LIGHT_MCDM2_cog" ("light at night", "night light", "light")
#' }
#' 
#' \strong{Climate Change}
#' \itemize{
#'   \item 17 - "5_1_TEMP_TRENDS_cog" ("temperature trends", "temp trends")
#'   \item 18 - "5_2_TEMP_SIGNIF_cog" ("temperature significance", "temp signif")
#'   \item 19 - "5_3_CLIM_EXTREME_cog" ("climate extremes")
#'   \item 20 - "5_4_CLIM_VELOCITY_cog" ("climate velocity", "velocity")
#'   \item 21 - "5_5_ARIDITY_TREND_cog" ("aridity trend", "aridity")
#' }
#'
#' \strong{Citation:}\cr
#' Branco VV, Capinha C, Rocha J, Correia L, Cardoso P (2024). "SPECTRE: standardized global spatial data on terrestrial SPecies and ECosystems ThREats." Global Ecology and Biogeography, 34, e13949.
#' \url{https://doi.org/10.1111/geb.13949}
#'
#' Note: Many SPECTRE variables are derived from external primary datasets.
#' Users should consult and cite the original sources listed in the SPECTRE
#' supplementary materials.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#'   spectre(vars=c("forest loss", "light at night"))
#'   }
#' @export
spectre <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using SPECTRE global threat layers.\n",
    "Citation: Branco VV, Capinha C, Rocha J, Correia L, Cardoso P (2024). SPECTRE: standardized global spatial data on terrestrial SPecies and ECosystems ThREats. Global Ecology and Biogeography, 34, e13949.\n",
    "DOI: {.url https://doi.org/10.1111/geb.13949}\n"
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
  spectre_lookup <- list(
    "1_1_MINING_AREA_cog"        = c("mining area", "mining_area", "mining"),
    "1_2_HAZARD_POTENTIAL_cog"   = c("hazard potential", "hazard"),
    "1_3_HUMAN_DENSITY_cog"      = c("human density", "population", "pop"),
    "1_4_BUILT_AREA_cog"         = c("built area", "built"),
    "1_5_ROAD_DENSITY_cog"       = c("road density", "roads", "road"),
    "1_6_FOOTPRINT_PERC_cog"     = c("human footprint", "footprint"),
    "1_7_IMPACT_AREA_cog"        = c("impacted area", "impact area"),
    "1_8_MODIF_AREA_cog"         = c("modified area", "modif area"),
    "1_9_HUMAN_BIOMES_cog"       = c("human biomes", "biomes"),
    "1_10_FIRE_OCCUR_cog"        = c("fires", "fire"),
    "1_11_CROP_PERC_UNI_cog"     = c("crops uni", "crop uni", "crop"),
    "1_12_CROP_PERC_IIASA_cog"   = c("crops iiasa", "iiasa crops"),
    "1_13_LIVESTOCK_MASS_cog"    = c("livestock", "livestock mass"),
    "2_1_FOREST_LOSS_PERC_cog"   = c("forest loss"),
    "2_2_FOREST_TREND_cog"       = c("forest trend"),
    "3_1_LIGHT_MCDM2_cog"        = c("light at night", "night light", "light"),
    "5_1_TEMP_TRENDS_cog"        = c("temperature trends", "temp trends"),
    "5_2_TEMP_SIGNIF_cog"        = c("temperature significance", "temp signif"),
    "5_3_CLIM_EXTREME_cog"       = c("climate extremes"),
    "5_4_CLIM_VELOCITY_cog"      = c("climate velocity", "velocity"),
    "5_5_ARIDITY_TREND_cog"      = c("aridity trend", "aridity")
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
  for (canon in names(spectre_lookup)) {
    for (syn in spectre_lookup[[canon]]) {
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
      "Unknown SPECTRE variables:",
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
       # fs::file_delete(dest_file)
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
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      
      rm(extracted)
      gc()
      if (!is_global) {
       # fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  base_url <- "https://www.nic.funet.fi/index/geodata/hy/spectre/2023"
  
  cli::cli_alert_info("Starting the download of SPECTRE data...")
  
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
    
    # write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}