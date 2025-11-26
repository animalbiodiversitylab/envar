# R/soil.R

#' Download and process Harmonized World Soil Database v2.0
#'
#' `soil()` downloads, processes, and extracts variables from the
#' **Harmonized World Soil Database v2.0 (HWSD v2.0)**.
#' The variable corresponds to a global raster file at 1 km resolution
#' representing soil types.
#'
#' The function allows users to input either:
#' - **canonical variable names**, e.g. `"hwsd"`
#' - **human-readable names**, e.g. `"soil"`, `"soil type"`, `"type"`, etc.
#'
#' It automatically:
#' - downloads the source data (zipped),
#' - unzips and processes the raster (`HWSD2.bil`),
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use HWSD v2.0 data, please cite:
#'
#' **FAO & IIASA. (2023).**
#' *Harmonized World Soil Database v2.0.* Food and Agriculture Organization of the United Nations, Rome and International Institute for Applied Systems Analysis, Laxenburg, Austria.
#' https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/
#'
#'
#' ## Available variables
#'
#' | Human-readable name          | Canonical variable code              |
#' |------------------------------|--------------------------------------|
#' | soil                         | hwsd                                 |
#' | type                         | hwsd                                 |
#' | soil type                    | hwsd                                 |
#' | soiltype                     | hwsd                                 |
#' | hwsd                         | hwsd                                 |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables. Defaults to `"hwsd"` if left empty.
#'          Accepts friendly names or canonical codes.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of the processed soil layer.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

soil <- function(x, vars = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Harmonized World Soil Database v2.0.\n",
    "Citation: FAO & IIASA. (2023). Harmonized World Soil Database v2.0.\n",
    "DOI: {.url https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/}\n"
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
  # There is effectively one variable, but we allow multiple synonyms
  soil_lookup <- list(
    "hwsd" = c("soil", "type", "soiltype", "soil type", "hwsd")
  )
  
  # Default to "hwsd" if vars is empty/NULL
  if (is.null(vars)) {
    vars <- "hwsd"
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
  for (canon in names(soil_lookup)) {
    for (syn in soil_lookup[[canon]]) {
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
      "Unknown HWSD variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var) {
    temp_dir <- fs::path_temp("envar/soil")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {var}}...")
    
    success <- download_file(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {var}} from {.url {url}}.")
      return(NULL)
    }
    
    # HWSD comes in a zip, so we must unzip it first
    cli::cli_alert_info("Unzipping {.val {basename(dest_file)}}...")
    unzip_dir <- file.path(temp_dir, "unzipped")
    fs::dir_create(unzip_dir)
    utils::unzip(dest_file, exdir = unzip_dir)
    
    # Target the .bil file specifically
    raster_file <- file.path(unzip_dir, "HWSD2.bil")
    
    if (!fs::file_exists(raster_file)) {
      cli::cli_alert_warning("Expected raster file {.val HWSD2.bil} not found in archive.")
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(raster_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {raster_file}}.")
        fs::file_delete(dest_file)
        fs::dir_delete(unzip_dir)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val HWSD2}...")
      
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
      
      cli::cli_alert_success("Processed and added {.val HWSD2} to stack.")
      
      rm(layer)
      gc()
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val HWSD2}...")
      
      # process_points must handle the direct raster path
      extracted <- try(process_points(file = raster_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val HWSD2}.")
        fs::file_delete(dest_file)
        fs::dir_delete(unzip_dir)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, 4)], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val HWSD2} successfully.")
      
      rm(extracted)
      gc()
      fs::file_delete(dest_file)
      fs::dir_delete(unzip_dir)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  # Since there is only one source file for this function, we define it directly.
  # The loop ensures structure consistency if multiple codes were requested (though redundant here).
  
  full_url <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip"
  
  cli::cli_alert_info("Starting the download of HWSD data...")
  
  for (canon in requested_codes) {
    # For this specific dataset, the filename/URL is constant regardless of the code
    filename <- "HWSD2_RASTER.zip"
    url <- full_url
    dest <- file.path(fs::path_temp("envar/soil"), filename)
    
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