# R/gdppast.R

#' Download and process Historical Real GDP and Electricity Consumption
#'
#' This function downloads, processes, and extracts variables from the
#' global 1km gridded revised real gross domestic product and electricity 
#' consumption dataset (1992–2019).
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Economic Metrics}
#' \itemize{
#'   \item "gdp" ("gross domestic product", "real gdp", "economy", "economic output", "gross product")
#' }
#' 
#' \strong{Energy Metrics}
#' \itemize{
#'   \item "electricity" ("electricity consumption", "energy", "energy consumption", "power", "ec", "electric")
#' }
#'
#' \strong{Years available}
#' \itemize{
#'   \item 1992 to 2019.
#' }
#'
#' \strong{Citation:}\cr
#' Chen J, Gao M, Cheng S et al (2022). "Global 1 km x 1 km gridded revised real gross domestic product and electricity consumption during 1992–2019 based on calibrated nighttime light data." Scientific Data 9, 202.
#' https://doi.org/10.1038/s41597-022-01322-5
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of variables to download ("gdp" or "electricity").
#' @param year Numeric vector of years to download. Available range: 1992-2019.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Get GDP for 2000 and 2010
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#'   gdppast(vars="gdp", year=c(2000, 2010))
#'
#' # Get Electricity and GDP for 2019
#' processed <- var_get(country= "Vietnam") %>% 
#'   gdppast(vars=c("electricity", "gdp"), year=2019)
#' }
#' @export

gdppast <- function(x, vars, year, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Historical GDP and Electricity Consumption layers.\n",
    "Citation: Chen J, Gao M, Cheng S et al (2022). Global 1 km x 1 km gridded revised real gross domestic product and electricity consumption during 1992–2019 based on calibrated nighttime light data. Scientific Data 9, 202.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-022-01322-5}\n"
  ))
  
  # Validate Year
  if (missing(year)) cli::cli_abort("Argument {.arg year} is required.")
  if (any(year < 1992 | year > 2019)) {
    cli::cli_abort("Years must be between 1992 and 2019.")
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
    # Economic Metrics
    "gdp" = c("gross domestic product", "real gdp", "economy", "economic output", "gross product", "gdp"),
    
    # Energy Metrics
    "electricity" = c("electricity consumption", "energy", "energy consumption", "power", "ec", "electric", "electricity")
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
  handle_file <- function(url, dest_file, canon, user_name, yr) {
    # Use a specific temp directory structure to handle the nested zips
    temp_base <- fs::path_temp("envar/gdppast")
    fs::dir_create(temp_base)
    
    # Define the big zip output path
    big_zip_name <- if(canon == "gdp") "Real_GDP.zip" else "Electricity.zip"
    big_zip_path <- file.path(temp_base, big_zip_name)
    
    # Step 1: Download the BIG zip only if it doesn't exist or is invalid
    # Cache the big zip within the session temp to avoid re-downloading 300MB+ for every year loop
    if (!file.exists(big_zip_path)) {
      cli::cli_alert_info("Downloading main archive for {.val {canon}}...")
      success <- download_file_figshare(url, big_zip_path)
      if (!success) {
        cli::cli_alert_warning("Failed to download main archive from {.url {url}}.")
        return(NULL)
      }
    }
    
    # Step 2: Extract the specific Year Zip from the Big Zip
    # Structure:
    # GDP: "updated real GDP/1992.zip"
    # Electricity: "updated electricity consumption/1992.zip"
    
    inner_folder <- if(canon == "gdp") "updated real GDP" else "updated electricity consumption"
    inner_zip_name <- paste0(yr, ".zip")
    path_inside_zip <- file.path(inner_folder, inner_zip_name)
    
    # Unzip strictly the specific year zip file
    unzip_success <- try(utils::unzip(big_zip_path, files = path_inside_zip, exdir = temp_base, overwrite = TRUE), silent=TRUE)
    
    extracted_inner_zip <- file.path(temp_base, inner_folder, inner_zip_name)
    
    if (inherits(unzip_success, "try-error") || !file.exists(extracted_inner_zip)) {
      cli::cli_alert_warning("Could not find year {.val {yr}} inside the archive.")
      return(NULL)
    }
    
    # Step 3: Extract the TIF from the Year Zip
    # Structure inside year zip:
    # GDP: "1992GDP.tif"
    # Electricity: "EC1992.tif"
    
    tif_name <- if(canon == "gdp") paste0(yr, "GDP.tif") else paste0("EC", yr, ".tif")
    
    # Unzip the final TIF
    utils::unzip(extracted_inner_zip, files = tif_name, exdir = temp_base, overwrite = TRUE)
    
    final_tif_path <- file.path(temp_base, tif_name)
    
    if (!file.exists(final_tif_path)) {
      cli::cli_alert_warning("Could not extract TIF {.val {tif_name}}.")
      return(NULL)
    }
    
    # Construct a descriptive name: e.g. "gdp_2000" or "electricity_1995"
    final_layer_name <- paste0(user_name, "_", yr)
    
    # Copy to standardized path for extr_check compatibility
    grids_dir <- fs::path_temp("envar/grids")
    fs::dir_create(grids_dir)
    dest_file <- file.path(grids_dir, paste0(final_layer_name, ".tif"))
    fs::file_copy(final_tif_path, dest_file, overwrite = TRUE)
    #fs::file_delete(final_tif_path)
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
         # fs::file_delete(dest_file)
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
      
      # Assign user-requested name to layer
      names(layer1) <- final_layer_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {final_layer_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      # Clean up the specific year TIF and inner zip, but keep big zip for next loop
      #fs::file_delete(final_tif_path)
      #fs::file_delete(extracted_inner_zip)
      
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
      # Clean up specific year files
      #fs::file_delete(final_tif_path)
      #fs::file_delete(extracted_inner_zip)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables AND years
  # --------------------------------------------------------------------
  
  cli::cli_alert_info("Processing GDP/Electricity data...")
  
  # Define base URLs for the massive zip files
  url_gdp <- "https://figshare.com/ndownloader/files/31456837"
  url_ec  <- "https://figshare.com/ndownloader/files/31456843"
  
  # Double loop: For every requested variable, loop through every requested year
  for (canon in requested_codes) {
    
    # Determine URL based on variable type
    if (canon == "gdp") {
      url <- url_gdp
    } else {
      url <- url_ec
    }
    
    user_name <- code_to_user_name[[canon]]
    
    for (yr in year) {
      # Pass canon, user_name, and year to the handler
      # The destination file argument is placeholder here, as the handler manages nested extraction
      dummy_dest <- file.path(fs::path_temp("envar/gdppast"), paste0(canon, "_", yr, ".tif"))
      
      handle_file(url, dummy_dest, canon, user_name, yr)
    }
  }
  
  # Cleanup: optionally remove the big zips here if we want to save space,
  # or leave them in temp for the session. Let's clean explicitly to be safe.
  # fs::dir_delete(fs::path_temp("envar/gdppast")) 
  # (Commented out to allow subsequent calls to be faster in same session, 
  # OS usually handles temp cleanup)
  
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
