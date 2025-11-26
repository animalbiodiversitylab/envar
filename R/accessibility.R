# R/accessibility.R

#' Download and process Global Accessibility Indicators
#'
#' `accessibility()` downloads, processes, and extracts variables from the
#' **Global Accessibility Indicators** dataset.
#' Each variable corresponds to a raster representing the travelling time (in minutes)
#' to cities or ports of specific sizes.
#'
#' The function allows users to input either:
#' - **canonical variable codes**, e.g. `"cities1"`, `"ports1"`
#' - **human-readable names**, e.g. `"large cities"`, `"travel time cities 1"`,
#'   `"small ports"`, `"ports 4"`, etc.
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
#' **Nelson, A., Weiss, D.J., van Etten, J. et al. (2019).**
#' *A suite of global accessibility indicators.* Sci Data **6**, 266.
#' https://doi.org/10.1038/s41597-019-0265-5
#'
#'
#' ## City Classifications
#'
#' | Code | Population minimum (>=) | Population maximum (<) |
#' |------|-------------------------|------------------------|
#' | 1    | 5,000,000               | 50,000,000             |
#' | 2    | 1,000,000               | 5,000,000              |
#' | 3    | 500,000                 | 1,000,000              |
#' | 4    | 200,000                 | 500,000                |
#' | 5    | 100,000                 | 200,000                |
#' | 6    | 50,000                  | 100,000                |
#' | 7    | 20,000                  | 50,000                 |
#' | 8    | 10,000                  | 20,000                 |
#' | 9    | 5,000                   | 10,000                 |
#' | 10   | 20,000                  | 110,000,000            |
#' | 11   | 50,000                  | 50,000,000             |
#' | 12   | 5,000                   | 110,000,000            |
#'
#' ## Port Classifications
#'
#' | Code | Port size   | Number of ports |
#' |------|-------------|-----------------|
#' | 1    | Large       | 160             |
#' | 2    | Medium      | 361             |
#' | 3    | Small       | 990             |
#' | 4    | Very small  | 2,153           |
#' | 5    | Any         | 3,778           |
#'
#'
#' ## Available variables
#'
#' | Human-readable name examples     | Canonical variable code |
#' |----------------------------------|-------------------------|
#' | cities 1, huge cities            | cities1                 |
#' | cities 2, large cities           | cities2                 |
#' | cities 3, medium cities          | cities3                 |
#' | cities 4                         | cities4                 |
#' | cities 5                         | cities5                 |
#' | cities 6                         | cities6                 |
#' | cities 7                         | cities7                 |
#' | cities 8                         | cities8                 |
#' | cities 9                         | cities9                 |
#' | cities 10                        | cities10                |
#' | cities 11                        | cities11                |
#' | cities 12                        | cities12                |
#' | ports 1, large ports             | ports1                  |
#' | ports 2, medium ports            | ports2                  |
#' | ports 3, small ports             | ports3                  |
#' | ports 4, very small ports        | ports4                  |
#' | ports 5, any port                | ports5                  |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed accessibility layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

accessibility <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Accessibility Indicators.\n",
    "Citation: Nelson, A., Weiss, D.J., van Etten, J. et al. (2019). A suite of global accessibility indicators. Sci Data 6, 266.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-019-0265-5}\n"
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
  # Data Source URLs (Figshare)
  # --------------------------------------------------------------------
  accessibility_urls <- c(
    "cities1"  = "https://figshare.com/ndownloader/files/14189804",
    "cities2"  = "https://figshare.com/ndownloader/files/14189807",
    "cities3"  = "https://figshare.com/ndownloader/files/14189810",
    "cities4"  = "https://figshare.com/ndownloader/files/14189816",
    "cities5"  = "https://figshare.com/ndownloader/files/14189819",
    "cities6"  = "https://figshare.com/ndownloader/files/14189825",
    "cities7"  = "https://figshare.com/ndownloader/files/14189831",
    "cities8"  = "https://figshare.com/ndownloader/files/14189837",
    "cities9"  = "https://figshare.com/ndownloader/files/14189840",
    "cities10" = "https://figshare.com/ndownloader/files/14189843",
    "cities11" = "https://figshare.com/ndownloader/files/14189849",
    "cities12" = "https://figshare.com/ndownloader/files/14189852",
    "ports1"   = "https://figshare.com/ndownloader/files/14189864",
    "ports2"   = "https://figshare.com/ndownloader/files/14189870",
    "ports3"   = "https://figshare.com/ndownloader/files/14189873",
    "ports4"   = "https://figshare.com/ndownloader/files/14189879",
    "ports5"   = "https://figshare.com/ndownloader/files/14189885"
  )
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  accessibility_lookup <- list(
    "cities1"  = c("cities 1", "city 1", "cities >5m", "huge cities", "travel time cities 1"),
    "cities2"  = c("cities 2", "city 2", "cities >1m", "large cities", "travel time cities 2"),
    "cities3"  = c("cities 3", "city 3", "medium cities", "travel time cities 3"),
    "cities4"  = c("cities 4", "city 4", "small cities", "travel time cities 4"),
    "cities5"  = c("cities 5", "city 5", "travel time cities 5"),
    "cities6"  = c("cities 6", "city 6", "travel time cities 6"),
    "cities7"  = c("cities 7", "city 7", "travel time cities 7"),
    "cities8"  = c("cities 8", "city 8", "towns", "travel time cities 8"),
    "cities9"  = c("cities 9", "city 9", "small towns", "travel time cities 9"),
    "cities10" = c("cities 10", "city 10", "aggregated cities 1", "travel time cities 10"),
    "cities11" = c("cities 11", "city 11", "aggregated cities 2", "travel time cities 11"),
    "cities12" = c("cities 12", "city 12", "aggregated cities 3", "travel time cities 12"),
    "ports1"   = c("ports 1", "port 1", "large ports", "travel time ports 1"),
    "ports2"   = c("ports 2", "port 2", "medium ports", "travel time ports 2"),
    "ports3"   = c("ports 3", "port 3", "small ports", "travel time ports 3"),
    "ports4"   = c("ports 4", "port 4", "very small ports", "travel time ports 4"),
    "ports5"   = c("ports 5", "port 5", "any port", "all ports", "travel time ports 5")
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
  for (canon in names(accessibility_lookup)) {
    for (syn in accessibility_lookup[[canon]]) {
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
      "Unknown Accessibility variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var) {
    temp_dir <- fs::path_temp("envar/grids")
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
      
      # Assign name to layer to ensure stack has useful names
      names(layer) <- var
      
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
      # Ensure proper naming of the extracted column
      if (ncol(extracted) >= 2) { 
        # Assuming process_points returns ID + Value, rename value col
        names(extracted)[ncol(extracted)] <- var 
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
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
  cli::cli_alert_info("Starting the download of Accessibility data...")
  
  for (canon in requested_codes) {
    # Get specific URL for this canonical code
    url <- accessibility_urls[[canon]]
    
    if (is.null(url)) {
      cli::cli_alert_warning("No URL found for {.val {canon}}.")
      next
    }
    
    filename <- paste0(canon, ".tif")
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
    handle_file(url=url,dest_file= dest, var=canon)
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