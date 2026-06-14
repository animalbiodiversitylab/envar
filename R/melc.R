# R/melc.R

#' Download and process Global 1 km Land Cover variables
#'
#' This function downloads, processes, and extracts variables from the
#' Global 1 km Land Cover dataset. Each variable corresponds to a global 
#' raster representing a specific land cover class or diversity index derived 
#' from very high-resolution imagery.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Land Cover Classes}
#' \itemize{
#'   \item 1 - "wetland" ("wetlands", "swamp", "marsh", "bog", "fen")
#'   \item 2 - "bare" ("bare ground", "bare soil", "desert", "unvegetated")
#'   \item 3 - "built" ("built area", "built up", "urban", "artificial", "impervious")
#'   \item 4 - "cropland" ("agriculture", "agricultural", "crop", "crops", "farming")
#'   \item 5 - "grass" ("grassland", "grass land", "meadow", "pasture", "prairie")
#'   \item 6 - "ice" ("snow", "snow and ice", "glacier", "ice", "permafrost")
#'   \item 8 - "mangrove" ("mangroves")
#'   \item 9 - "moss" ("mosses", "lichen", "lichens", "moss and lichen")
#'   \item 10 - "shrub" ("shrubland", "scrub", "bush", "thicket")
#'   \item 11 - "tree" ("trees", "forest", "woodland", "canopy", "canopy cover")
#'   \item 12 - "water" ("surface water", "lake", "river", "freshwater")
#' }
#' 
#' \strong{Diversity & Metrics}
#' \itemize{
#'   \item 7 - "land_perc" ("percentage of land", "land percentage", "land cover fraction", "land fraction")
#'   \item 13 - "simpson" ("simpson index", "diversity simpson", "simpson diversity")
#'   \item 14 - "shannon" ("shannon index", "entropy", "shannon entropy", "shannon diversity")
#'   \item 15 - "evenness" ("evenness index", "pielou", "pielou evenness", "species evenness")
#' }
#'
#' \strong{Citation:}\cr
#' Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025). "Global 1 km land cover for macroecological modelling from very high resolution imagery." Figshare.
#' https://doi.org/10.6084/m9.figshare.30665069
#'
#' Note: Users should verify the terms of use provided at https://figshare.com/s/4e7dee46628b530aee03
#' 
#' @param x The output from `par_set()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param discover Logical. If TRUE, creates a discovery map (unused in current implementation but kept for compatibility).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- par_set(country= "Italy", crs=3035) %>% 
#'   melc(vars=c("tree", "water"))
#' }
#' @export

melc <- function(x, vars, discover=TRUE, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global 1 km Land Cover variables.\n",
    "Citation: Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025). Global 1 km land cover for macroecological modelling from very high resolution imagery. Figshare.\n",
    "DOI: {.url https://doi.org/10.6084/m9.figshare.30665069}\n"
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
  esa_lookup <- list(
    "wetland"   = c("wetlands", "swamp", "marsh", "bog", "fen"),
    "bare"      = c("bare ground", "bare soil", "desert", "unvegetated"),
    "built"     = c("built area", "built up", "urban", "artificial", "impervious"),
    "cropland"  = c("agriculture", "agricultural", "crop", "crops", "farming"),
    "grass"     = c("grassland", "grass land", "meadow", "pasture", "prairie"),
    "ice"       = c("snow", "snow and ice", "glacier", "ice", "permafrost"),
    "land_perc" = c("percentage of land", "land percentage", "land cover fraction", "land fraction"),
    "mangrove"  = c("mangroves", "mangrove"),
    "moss"      = c("mosses", "lichen", "moss", "lichens", "moss and lichen"),
    "shrub"     = c("shrubland", "shrub", "scrub", "bush", "thicket"),
    "tree"      = c("trees", "tree", "forest", "woodland", "canopy", "canopy cover"),
    "water"     = c("surface water", "water", "lake", "river", "freshwater"),
    "simpson"   = c("simpson index","simpson", "diversity simpson", "simpson diversity"),
    "shannon"   = c("shannon index", "entropy", "shannon", "shannon entropy", "shannon diversity"),
    "evenness"  = c("evenness index", "evenness", "pielou", "pielou evenness", "species evenness")
  )
  
  # Direct URL lookup
  url_lookup <- list(
    "wetland"   = "https://figshare.com/ndownloader/files/59720123?private_link=4e7dee46628b530aee03",
    "bare"      = "https://figshare.com/ndownloader/files/59720138?private_link=4e7dee46628b530aee03",
    "built"     = "https://figshare.com/ndownloader/files/59720117?private_link=4e7dee46628b530aee03",
    "cropland"  = "https://figshare.com/ndownloader/files/59720132?private_link=4e7dee46628b530aee03",
    "grass"     = "https://figshare.com/ndownloader/files/59720153?private_link=4e7dee46628b530aee03",
    "ice"       = "https://figshare.com/ndownloader/files/59720108?private_link=4e7dee46628b530aee03",
    "land_perc" = "https://figshare.com/ndownloader/files/59720111?private_link=4e7dee46628b530aee03",
    "mangrove"  = "https://figshare.com/ndownloader/files/59720099?private_link=4e7dee46628b530aee03",
    "moss"      = "https://figshare.com/ndownloader/files/59720126?private_link=4e7dee46628b530aee03",
    "shrub"     = "https://figshare.com/ndownloader/files/59720141?private_link=4e7dee46628b530aee03",
    "tree"      = "https://figshare.com/ndownloader/files/59720150?private_link=4e7dee46628b530aee03",
    "water"     = "https://figshare.com/ndownloader/files/59720129?private_link=4e7dee46628b530aee03",
    "simpson"   = "https://figshare.com/ndownloader/files/59720171?private_link=4e7dee46628b530aee03",
    "shannon"   = "https://figshare.com/ndownloader/files/59720168?private_link=4e7dee46628b530aee03",
    "evenness"  = "https://figshare.com/ndownloader/files/59720165?private_link=4e7dee46628b530aee03"
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
  
  for (canon in names(esa_lookup)) {
    for (syn in esa_lookup[[canon]]) {
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
      "Unknown land cover variables:",
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
        # if (!is_global) {
        #   fs::file_delete(dest_file)
        # }
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
#      if (!is_global) {
#        fs::file_delete(dest_file)
#     }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        # if (!is_global) {
        #   fs::file_delete(dest_file)
        # }
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
  
  cli::cli_alert_info("Starting the download of ESA global land cover data...")
  
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
    
    # Remove NAs if necessary
    if (isTRUE(set_na)){
      
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
      
      # # Preserve CRS from previous extraction
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