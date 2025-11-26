# R/cloudcover.R

#' Download and process EarthEnv Global Cloud Cover layers
#'
#' `cloudcover()` downloads, processes, and extracts variables from the
#' **EarthEnv Global Cloud Cover** dataset. Each variable corresponds to a 
#' global Cloud-Optimized GeoTIFF (COG) representing cloud cover dynamics.
#'
#' The function allows users to input either:
#' - **canonical EarthEnv filenames**, e.g. `"MODCF_meanannual"`
#' - **human-readable names**, e.g. `"mean annual"`, `"cloud forest"`,
#'   `"seasonality concentration"`, `"january"`, etc.
#'
#' It automatically:
#' - downloads the selected files,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use this data, please cite:
#'
#' **Wilson AM, Jetz W (2016)** #' *Remotely Sensed High-Resolution Global Cloud Dynamics for Predicting 
#' Ecosystem and Biodiversity Distributions.* PLoS Biol 14(3): e1002415. 
#' https://doi.org/10.1371/journal.pbio.1002415
#'
#'
#' ## Available variables
#'
#' | Human-readable name          | Canonical variable code              |
#' |------------------------------|--------------------------------------|
#' | cloud forest prediction      | MODCF_CloudForestPrediction          |
#' | inter-annual variability     | MODCF_interannualSD                  |
#' | intra-annual variability     | MODCF_intraannualSD                  |
#' | mean annual                  | MODCF_meanannual                     |
#' | january mean (jan)           | MODCF_monthlymean_01                 |
#' | february mean (feb)          | MODCF_monthlymean_02                 |
#' | march mean (mar)             | MODCF_monthlymean_03                 |
#' | april mean (apr)             | MODCF_monthlymean_04                 |
#' | may mean                     | MODCF_monthlymean_05                 |
#' | june mean (jun)              | MODCF_monthlymean_06                 |
#' | july mean (jul)              | MODCF_monthlymean_07                 |
#' | august mean (aug)            | MODCF_monthlymean_08                 |
#' | september mean (sep)         | MODCF_monthlymean_09                 |
#' | october mean (oct)           | MODCF_monthlymean_10                 |
#' | november mean (nov)          | MODCF_monthlymean_11                 |
#' | december mean (dec)          | MODCF_monthlymean_12                 |
#' | seasonality concentration    | MODCF_seasonality_concentration      |
#' | seasonality rgb              | MODCF_seasonality_rgb                |
#' | seasonality theta            | MODCF_seasonality_theta              |
#' | seasonality visual (visct)   | MODCF_seasonality_visct              |
#' | spatial variability (1 deg)  | MODCF_spatialSD_1deg                 |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed cloud cover layers.  
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

cloudcover <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Global Cloud Cover layers.\n",
    "Citation: Wilson AM, Jetz W (2016). PLoS Biol 14(3): e1002415.\n",
    "DOI: {.url https://doi.org/10.1371/journal.pbio.1002415}\n",
    "Note: Please cite original sources of primary datasets where appropriate."
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
  
  # Normalizer
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
      "Unknown Cloud Cover variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var) {
    temp_dir <- fs::path_temp("envar/cloud")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {var}}...")
    
    success <- download_file(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {var}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {basename(dest_file)}}...")
      
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
      
      cli::cli_alert_success("Processed and added {.val {basename(dest_file)}} to stack.")
      
      rm(layer)
      gc()
      fs::file_delete(dest_file)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {basename(dest_file)}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {basename(dest_file)}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, 4)], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {basename(dest_file)}} successfully.")
      
      rm(extracted)
      gc()
      fs::file_delete(dest_file)
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
    dest <- file.path(fs::path_temp("envar/cloud"), filename)
    
    handle_file(url, dest, canon)
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