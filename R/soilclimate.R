# R/soilclimate.R

#' Download and process Soil Bioclimatic and Temperature layers
#'
#' This function downloads, processes, and extracts soil bioclimatic variables 
#' and monthly soil temperatures from the Global Soil Temperature dataset 
#' (Lembrechts et al., 2022).
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Bioclimatic Variables}
#' \itemize{
#'   \item SBIO1 - "SBIO1" [°C] ("annual mean temperature", "annual mean", "amt")
#'   \item SBIO2 - "SBIO2" [°C] ("mean diurnal range", "mean diurnal", "mdr")
#'   \item SBIO3 - "SBIO3" [Ratio] ("isothermality")
#'   \item SBIO4 - "SBIO4" [SD] ("temperature seasonality", "seasonality")
#'   \item SBIO5 - "SBIO5" [°C] ("max temperature warmest month", "max temp", "warmest month")
#'   \item SBIO6 - "SBIO6" [°C] ("min temperature coldest month", "min temp", "coldest month")
#'   \item SBIO7 - "SBIO7" [°C] ("temperature annual range", "annual range", "tar")
#'   \item SBIO8 - "SBIO8" [°C] ("mean temperature wettest quarter", "wettest quarter")
#'   \item SBIO9 - "SBIO9" [°C] ("mean temperature driest quarter", "driest quarter")
#'   \item SBIO10 - "SBIO10" [°C] ("mean temperature warmest quarter", "warmest quarter")
#'   \item SBIO11 - "SBIO11" [°C] ("mean temperature coldest quarter", "coldest quarter")
#' }
#'
#' \strong{Monthly Mean Soil Temperatures}
#' \itemize{
#'   \item soilT01 - "soilT01" [°C] ("january mean", "january", "jan")
#'   \item soilT02 - "soilT02" [°C] ("february mean", "february", "feb")
#'   \item soilT03 - "soilT03" [°C] ("march mean", "march", "mar")
#'   \item soilT04 - "soilT04" [°C] ("april mean", "april", "apr")
#'   \item soilT05 - "soilT05" [°C] ("may mean", "may")
#'   \item soilT06 - "soilT06" [°C] ("june mean", "june", "jun")
#'   \item soilT07 - "soilT07" [°C] ("july mean", "july", "jul")
#'   \item soilT08 - "soilT08" [°C] ("august mean", "august", "aug")
#'   \item soilT09 - "soilT09" [°C] ("september mean", "september", "sep")
#'   \item soilT10 - "soilT10" [°C] ("october mean", "october", "oct")
#'   \item soilT11 - "soilT11" [°C] ("november mean", "november", "nov")
#'   \item soilT12 - "soilT12" [°C] ("december mean", "december", "dec")
#' }
#'
#' \strong{Citation:}\cr
#' Lembrechts, Jonas J., et al. (2022).
#' "Global maps of soil temperature." Global Change Biology 28, no. 9: 3110-3144.
#' \url{https://doi.org/10.1111/gcb.16060}
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#' 
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param depth Character string defining the soil depth range. 
#' Options are "0-5" (default) or "5-15".
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#' soilclimate(vars=c("SBIO1", "SBIO10"), depth="5-15")
#'    }
#' @export

soilclimate <- function(x, vars, depth = "0-5", ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global maps of soil temperature.\n",
    "Citation: Lembrechts JJ et al. (2022). Global Change Biology 28(9): 3110-3144.\n",
    "DOI: {.url https://doi.org/10.1111/gcb.16060}\n"
  ))
  
  # Validate depth argument
  if (!depth %in% c("0-5", "5-15")) {
    cli::cli_abort(c(
      "Invalid depth argument.",
      "i" = "Supported depths are: {.val 0-5} and {.val 5-15}."
    ))
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
  soil_lookup <- list(
    # Bioclimatic Variables
    "SBIO1"  = c("annual mean temperature", "annual mean", "amt"),
    "SBIO2"  = c("mean diurnal range", "mean diurnal", "mdr"),
    "SBIO3"  = c("isothermality", "iso"),
    "SBIO4"  = c("temperature seasonality", "seasonality", "temp seasonality"),
    "SBIO5"  = c("max temperature warmest month", "max temp", "warmest month"),
    "SBIO6"  = c("min temperature coldest month", "min temp", "coldest month"),
    "SBIO7"  = c("temperature annual range", "annual range", "tar"),
    "SBIO8"  = c("mean temperature wettest quarter", "wettest quarter"),
    "SBIO9"  = c("mean temperature driest quarter", "driest quarter"),
    "SBIO10" = c("mean temperature warmest quarter", "warmest quarter"),
    "SBIO11" = c("mean temperature coldest quarter", "coldest quarter"),
    
    # Monthly Means
    "soilT01" = c("january mean", "january", "jan"),
    "soilT02" = c("february mean", "february", "feb"),
    "soilT03" = c("march mean", "march", "mar"),
    "soilT04" = c("april mean", "april", "apr"),
    "soilT05" = c("may mean", "may"),
    "soilT06" = c("june mean", "june", "jun"),
    "soilT07" = c("july mean", "july", "jul"),
    "soilT08" = c("august mean", "august", "aug"),
    "soilT09" = c("september mean", "september", "sep"),
    "soilT10" = c("october mean", "october", "oct"),
    "soilT11" = c("november mean", "november", "nov"),
    "soilT12" = c("december mean", "december", "dec")
  )
  
  # Mapping from Canonical Code to Filename Base (Zenodo naming convention)
  filename_base_map <- list(
    "SBIO1"  = "SBIO1_Annual_Mean_Temperature",
    "SBIO2"  = "SBIO2_Mean_Diurnal_Range",
    "SBIO3"  = "SBIO3_Isothermality",
    "SBIO4"  = "SBIO4_Temperature_Seasonality",
    "SBIO5"  = "SBIO5_Max_Temperature_of_Warmest_Month",
    "SBIO6"  = "SBIO6_Min_Temperature_of_Coldest_Month",
    "SBIO7"  = "SBIO7_Temperature_Annual_Range",
    "SBIO8"  = "SBIO8_Mean_Temperature_of_Wettest_Quarter",
    "SBIO9"  = "SBIO9_Mean_Temperature_of_Driest_Quarter",
    "SBIO10" = "SBIO10_Mean_Temperature_of_Warmest_Quarter",
    "SBIO11" = "SBIO11_Mean_Temperature_of_Coldest_Quarter",
    "soilT01" = "soilT_1",
    "soilT02" = "soilT_2",
    "soilT03" = "soilT_3",
    "soilT04" = "soilT_4",
    "soilT05" = "soilT_5",
    "soilT06" = "soilT_6",
    "soilT07" = "soilT_7",
    "soilT08" = "soilT_8",
    "soilT09" = "soilT_9",
    "soilT10" = "soilT_10",
    "soilT11" = "soilT_11",
    "soilT12" = "soilT_12"
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
  for (canon in names(soil_lookup)) {
    for (syn in soil_lookup[[canon]]) {
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
      "Unknown Soil Climate variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/soil")
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
        #fs::file_delete(dest_file)
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
        #fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  base_url <- "https://zenodo.org/records/4558732/files"
  
  # Format depth string for filename (0-5 -> 0_5cm)
  depth_str <- paste0(gsub("-", "_", depth), "cm")
  
  cli::cli_alert_info(paste0("Processing Global Soil Temperature data (Depth: ", depth, ")..."))
  
  for (canon in requested_codes) {
    # Construct filename based on map and depth
    # Structure: {BaseName}_{Depth}.tif
    file_base <- filename_base_map[[canon]]
    filename <- paste0(file_base, "_", depth_str, ".tif")
    
    # Zenodo requires ?download=1 appended to the URL
    url <- paste0(file.path(base_url, filename), "?download=1")
    
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    
    # Destination file - use user_name for extr_check compatibility
    dest <- file.path(fs::path_temp("envar/grids"), paste0(user_name, ".tif"))
    
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