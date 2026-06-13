# R/cloudcover.R

#' Download and process EarthEnv Global Cloud Cover layers
#'
#' This function downloads, processes, and extracts variables from the
#' EarthEnv Global Cloud Cover dataset. Each variable corresponds to a 
#' global Cloud-Optimized GeoTIFF (COG) representing cloud cover dynamics.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Metrics}
#' \itemize{
#'   \item 1 - "MODCF_CloudForestPrediction" ("cloud forest prediction", "cloud forest", "cfp")
#'   \item 2 - "MODCF_interannualSD" ("inter-annual variability", "interannual sd", "interannual variability")
#'   \item 3 - "MODCF_intraannualSD" ("intra-annual variability", "intraannual sd", "intraannual variability")
#'   \item 4 - "MODCF_meanannual" ("mean annual", "annual mean", "annual")
#'   \item 5 - "MODCF_spatialSD_1deg" ("spatial variability", "spatial sd", "spatial sd 1deg")
#' }
#'
#' \strong{Seasonality}
#' \itemize{
#'   \item 6 - "MODCF_seasonality_concentration" ("seasonality concentration", "concentration")
#'   \item 7 - "MODCF_seasonality_rgb" ("seasonality rgb", "rgb")
#'   \item 8 - "MODCF_seasonality_theta" ("seasonality theta", "theta")
#'   \item 9 - "MODCF_seasonality_visct" ("seasonality single band", "seasonality visct", "seasonality color")
#' }
#'
#' \strong{Monthly Means}
#' \itemize{
#'   \item 10 - "MODCF_monthlymean_01" ("january mean", "january", "jan")
#'   \item 11 - "MODCF_monthlymean_02" ("february mean", "february", "feb")
#'   \item 12 - "MODCF_monthlymean_03" ("march mean", "march", "mar")
#'   \item 13 - "MODCF_monthlymean_04" ("april mean", "april", "apr")
#'   \item 14 - "MODCF_monthlymean_05" ("may mean", "may")
#'   \item 15 - "MODCF_monthlymean_06" ("june mean", "june", "jun")
#'   \item 16 - "MODCF_monthlymean_07" ("july mean", "july", "jul")
#'   \item 17 - "MODCF_monthlymean_08" ("august mean", "august", "aug")
#'   \item 18 - "MODCF_monthlymean_09" ("september mean", "september", "sep")
#'   \item 19 - "MODCF_monthlymean_10" ("october mean", "october", "oct")
#'   \item 20 - "MODCF_monthlymean_11" ("november mean", "november", "nov")
#'   \item 21 - "MODCF_monthlymean_12" ("december mean", "december", "dec")
#' }
#'
#' \strong{Citation:}\cr
#' Wilson AM, Jetz W (2016). "Remotely sensed high-resolution global cloud dynamics for predicting ecosystem and biodiversity distributions." PLoS Biol 14(3): e1002415.
#' https://doi.org/10.1371/journal.pbio.1002415
#'
#' Note: Please cite original sources of primary datasets where appropriate.
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
#' cloudcover(vars=c("mean annual", "january"))
#'   }
#' @export

cloudcover <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Global Cloud Cover layers.\n",
    "Citation: Wilson AM, Jetz W (2016). Remotely sensed high-resolution global cloud dynamics for predicting ecosystem and biodiversity distributions. PLoS Biol 14(3): e1002415.\n",
    "DOI: {.url https://doi.org/10.1371/journal.pbio.1002415}\n"
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
  cloud_lookup <- list(
    # Metrics
    "MODCF_CloudForestPrediction"     = c("cloud forest prediction", "cloud forest", "cfp"),
    "MODCF_interannualSD"             = c("inter-annual variability", "interannual sd", "interannual variability"),
    "MODCF_intraannualSD"             = c("intra-annual variability", "intraannual sd", "intraannual variability"),
    "MODCF_meanannual"                = c("mean annual", "annual mean", "annual"),
    "MODCF_spatialSD_1deg"            = c("spatial variability", "spatial sd", "spatial sd 1deg"),
    
    # Seasonality
    "MODCF_seasonality_concentration" = c("seasonality concentration", "concentration"),
    "MODCF_seasonality_rgb"           = c("seasonality rgb", "rgb"),
    "MODCF_seasonality_theta"         = c("seasonality theta", "theta"),
    "MODCF_seasonality_visct"         = c("seasonality single band", "seasonality visct", "seasonality color"),
    
    # Monthly means
    "MODCF_monthlymean_01"            = c("january mean", "january", "jan"),
    "MODCF_monthlymean_02"            = c("february mean", "february", "feb"),
    "MODCF_monthlymean_03"            = c("march mean", "march", "mar"),
    "MODCF_monthlymean_04"            = c("april mean", "april", "apr"),
    "MODCF_monthlymean_05"            = c("may mean", "may"),
    "MODCF_monthlymean_06"            = c("june mean", "june", "jun"),
    "MODCF_monthlymean_07"            = c("july mean", "july", "jul"),
    "MODCF_monthlymean_08"            = c("august mean", "august", "aug"),
    "MODCF_monthlymean_09"            = c("september mean", "september", "sep"),
    "MODCF_monthlymean_10"            = c("october mean", "october", "oct"),
    "MODCF_monthlymean_11"            = c("november mean", "november", "nov"),
    "MODCF_monthlymean_12"            = c("december mean", "december", "dec")
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
  for (canon in names(cloud_lookup)) {
    for (syn in cloud_lookup[[canon]]) {
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
      "Unknown Cloud Cover variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/cloud")
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
  base_url <- "https://data.earthenv.org/cloud"
  
  cli::cli_alert_info("Processing EarthEnv Cloud data...")
  
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
