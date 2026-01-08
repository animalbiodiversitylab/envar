# R/worldclim.R

#' Download and process WorldClim Climate Data (Historical & Future)
#'
#' This function downloads, processes, and extracts variables from the
#' WorldClim climate dataset. Each variable corresponds to a global raster
#' representing climate variables at approximately 1-km resolution.
#' It supports both Historical (v2.1, 1970-2000) and Future (CMIP6) data.
#'
#' Available variables (working synonyms in parentheses):
#'
#' Temperature:
#' 
#' 1 - "tmin" ("min temp")
#' 
#' 2 - "tmax" ("max temp")
#' 
#' 3 - "tavg" ("average temp")
#' 
#' Precipitation:
#' 
#' 4 - "prec" ("precipitation", "pr")
#' 
#' Physical:
#' 
#' 5 - "srad" ("solar radiation")
#' 
#' 6 - "wind" ("wind speed")
#' 
#' 7 - "vapr" ("water vapor")
#' 
#' 8 - "elev" ("elevation")
#' 
#' Bioclimatic:
#' 
#' 9 - "bio" (all 19 bioclimatic variables), or specific e.g., "bio1", "bio12"
#'
#' Citation:
#'
#' Fick, Stephen E., and Robert J. Hijmans. "WorldClim 2: new 1-km spatial 
#' resolution climate surfaces for global land areas." International Journal 
#' of Climatology 37, no. 12 (2017): 4302-4315.
#' https://doi.org/10.1002/joc.5086
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param years Character vector of years or periods. For Historical: use "1970-2000" 
#'        or "historical". For Future: use "2021-2040", "2041-2060", "2061-2080", "2081-2100".
#' @param months Numeric vector (1-12) specifying which months to download. Only applies
#'        to monthly historical variables. Ignored for bioclimatic variables, elevation, 
#'        or future data.
#' @param gcm Character vector of General Circulation Models (for Future data).
#' @param ssp Character or numeric vector of Shared Socioeconomic Pathways (e.g., "126", "585").
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' worldclim(vars=c("tmin", "bio1"), years="1970-2000")
#'   }
#' @export

worldclim <- function(x, vars, years = NULL, months = NULL, gcm = NULL, rcp = NULL, 
                      ssp = NULL, ...) {
  
  old_timeout <- getOption("timeout")
  options(timeout = max(100000000000000, old_timeout))
  on.exit(options(timeout = old_timeout))
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using WorldClim.\n",
    "Citation: Fick, S. E. and Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. International Journal of Climatology.\n",
    "DOI: {.url https://doi.org/10.1002/joc.5086}\n"
  ))
  
  cli::cli_alert_info("Starting the download of WorldClim data...")
  
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
    path = par_list$path
    crs  <- par_list$crs
    is_global <- FALSE
    is_raster_input <- FALSE
    current_global_extent <- NULL
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Helper: Process a local file (Crop, Mask, Stack/Extract)
  # --------------------------------------------------------------------
  process_layer <- function(file_path, layer_name) {
    if (is_raster_input) {
      layer <- try(terra::rast(file_path), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {basename(file_path)}}.")
        return(FALSE)
      }
      
      cli::cli_alert_info("Processing layer {.val {layer_name}}...")
      
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
      
      # Assign name to layer
      names(layer1) <- layer_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {layer_name}} to stack.")
      rm(layer, layer1)
      gc()
      return(TRUE)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {layer_name}}...")
      
      extracted <- try(process_points(file = file_path, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {layer_name}}.")
        return(FALSE)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        names(extracted)[ncol(extracted)] <- layer_name
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {layer_name}} successfully.")
      rm(extracted)
      gc()
      return(TRUE)
    }
  }
  
  # --------------------------------------------------------------------
  # Helper: Download and Process (Direct TIF)
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, layer_name) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    success <- download_file(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {layer_name}} from {.url {url}}.")
      return(NULL)
    }
    
    process_layer(dest_file, layer_name)
    if (!is_global) {
     # fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # --- Variable Dictionary & Cleaning ---
  # --------------------------------------------------------------------
  req_vars <- unique(tolower(vars))
  clean_vars <- list()
  
  # Logic to handle specific bio requests (e.g. bio1) vs category (bio)
  for (v in req_vars) {
    if (v %in% c("tmin", "min temp")) clean_vars[["tmin"]] <- "tmin"
    else if (v %in% c("tmax", "max temp")) clean_vars[["tmax"]] <- "tmax"
    else if (v %in% c("tavg", "average temp")) clean_vars[["tavg"]] <- "tavg"
    else if (v %in% c("prec", "precipitation", "pr")) clean_vars[["prec"]] <- "prec"
    else if (v %in% c("srad", "solar radiation")) clean_vars[["srad"]] <- "srad"
    else if (v %in% c("wind", "wind speed")) clean_vars[["wind"]] <- "wind"
    else if (v %in% c("vapr", "water vapor")) clean_vars[["vapr"]] <- "vapr"
    else if (v %in% c("elev", "elevation")) clean_vars[["elev"]] <- "elev"
    else if (grepl("^bio", v)) {
      # If just "bio", keep "bio" (implies all). If "bio1", keep "bio1".
      clean_vars[[v]] <- "bio" 
    }
  }
  
  # Unique categories to download (e.g. if bio1 and bio2 asked, we download bio zip once)
  download_categories <- unique(unlist(clean_vars))
  
  # --------------------------------------------------------------------
  # --- Historical (1970-2000) Processing ---
  # --------------------------------------------------------------------
  # Check if years implies historical
  is_historical <- any(c("1970-2000", "historical") %in% tolower(as.character(years)))
  
  if (is_historical) {
    cli::cli_alert_info("Processing Historical WorldClim data (1970-2000)...")
    base_url_hist <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
    
    for (cat in download_categories) {
      
      # Construct ZIP URL
      zip_name <- sprintf("wc2.1_30s_%s.zip", cat)
      url <- sprintf("%s/%s", base_url_hist, zip_name)
      dest_zip <- file.path(fs::path_temp("envar/grids"), zip_name)
      
      # Download ZIP
      success <- download_file(url, dest_zip)
      if (!success) {
        cli::cli_alert_warning("Failed to download {.val {cat}} zip.")
        next
      }
      
      # Identify files to extract from zip
      files_in_zip <- unzip(dest_zip, list = TRUE)$Name
      files_to_extract <- c()
      
      if (cat == "elev") {
        files_to_extract <- files_in_zip # Only one file usually
      } else if (cat == "bio") {
        # Check what bio vars user asked for. If "bio", get all. If "bio1", get bio_1.tif
        user_bios <- names(clean_vars)[clean_vars == "bio"]
        if ("bio" %in% user_bios) {
          files_to_extract <- files_in_zip # Get all
        } else {
          # Extract specific numbers (e.g. wc2.1_30s_bio_1.tif)
          for (ub in user_bios) {
            # numeric part
            num <- gsub("bio", "", ub)
            pattern <- paste0("bio_", num, ".tif$")
            match <- grep(pattern, files_in_zip, value = TRUE)
            files_to_extract <- c(files_to_extract, match)
          }
        }
      } else {
        # Monthly variables (tmin, prec, etc.)
        # Check months argument
        ms <- if (is.null(months)) sprintf("%02d", 1:12) else sprintf("%02d", months)
        for (m in ms) {
          # Pattern: wc2.1_30s_tmin_01.tif
          pattern <- paste0("_", m, ".tif$")
          match <- grep(pattern, files_in_zip, value = TRUE)
          files_to_extract <- c(files_to_extract, match)
        }
      }
      
      # Unzip specific files
      if (length(files_to_extract) > 0) {
        unzip(dest_zip, files = files_to_extract, exdir = fs::path_temp("envar/grids"))
        
        # Process extracted files
        for (f in files_to_extract) {
          full_path <- file.path(fs::path_temp("envar/grids"), f)
          # Use original file name as layer name
          layer_name <- tools::file_path_sans_ext(basename(f))
          process_layer(full_path, layer_name)
          if (!is_global) {
          #  fs::file_delete(full_path)
          }
        }
      } else {
        cli::cli_alert_warning("No matching files found in zip for {.val {cat}} based on selection.")
      }
      
      # Delete ZIP
      if (!is_global) {
       # fs::file_delete(dest_zip)
      }
    }
    
    # Remove historical marker from years to prevent trying to find it in Future loop
    years <- setdiff(years, c("1970-2000", "historical"))
  }
  
  # --------------------------------------------------------------------
  # --- Future CMIP6 Processing ---
  # --------------------------------------------------------------------
  if (!is.null(years) && length(years) > 0) {
    
    # Clean vars for Future (only supports tmin, tmax, prec, bio)
    future_vars <- intersect(names(clean_vars), c("tmin", "tmax", "prec", "bio"))
    # Map back to clean names
    future_cats <- unique(unlist(clean_vars[future_vars]))
    
    # Normalize SSP
    if (!is.null(ssp)) {
      ssp_clean <- as.character(ssp)
      ssp_clean <- ifelse(grepl("^ssp", ssp_clean), ssp_clean, paste0("ssp", ssp_clean))
    }
    
    base_url_future <- "https://geodata.ucdavis.edu/cmip6/30s"
    
    for (g in gcm) {
      for (s in ssp_clean) {
        for (y in years) {
          
          # Skip if year is not valid future range
          if (!y %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100")) {
            next
          }
          
          for (cat in future_cats) {
            # For Future, "bioc" in URL is used for bio variables
            url_cat <- if (cat == "bio") "bioc" else cat
            
            filename <- sprintf("wc2.1_30s_%s_%s_%s_%s.tif", url_cat, g, s, y)
            url <- sprintf("%s/%s/%s/%s", base_url_future, g, s, filename)
            
            # Use descriptive layer name
            layer_name <- tools::file_path_sans_ext(filename)
            dest_file <- file.path(fs::path_temp("envar/grids"), paste0(layer_name, ".tif"))
            
            handle_file(url, dest_file, layer_name)
          }
        }
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
