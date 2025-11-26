# R/protection.R

#' Download and process WDPA Protected Area layers
#'
#' `protection()` downloads, processes, and extracts variables from the
#' **World Database of Protected Areas (WDPA)**. Each variable corresponds 
#' to a global raster representing different IUCN Management Categories of 
#' protected areas.
#'
#' The function allows users to input either:
#' - **canonical WDPA codes**, e.g. `"WDPA_II"`, `"WDPA_ALL"`
#' - **human-readable names**, e.g. `"national park"`, `"strict nature reserve"`,
#'   `"wilderness area"`, `"sustainable use"`, etc.
#'
#' It automatically:
#' - downloads the selected files from the source repositories,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use these data, please cite:
#'
#' **Protected Planet (2025).** #' *World Database of Protected Areas (WDPA).* #' https://www.protectedplanet.net/en
#'
#' _Note: Users should ensure they comply with the terms of use of the WDPA
#' when using these data for commercial or research purposes._
#'
#'
#' ## Available variables
#'
#' | Human-readable name                | Canonical variable code              | Description |
#' |------------------------------------|--------------------------------------|-------------|
#' | strict nature reserve (1a)         | WDPA_IA                              | IUCN Category Ia |
#' | wilderness area (1b)               | WDPA_IB                              | IUCN Category Ib |
#' | national park (2)                  | WDPA_II                              | IUCN Category II |
#' | natural monument (3)               | WDPA_III                             | IUCN Category III |
#' | habitat species management (4)     | WDPA_IV                              | IUCN Category IV |
#' | protected landscape (5)            | WDPA_V                               | IUCN Category V |
#' | sustainable use (6)                | WDPA_VI                              | IUCN Category VI |
#' | all protected areas                | WDPA_ALL                             | Combined/Full WDPA |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed WDPA layers.  
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

protection <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using World Database of Protected Areas (WDPA) layers.\n",
    "Citation: Protected Planet (2025). World Database of Protected Areas (WDPA).\n",
    "DOI: {.url https://www.protectedplanet.net/en}\n"
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
  protection_lookup <- list(
    "WDPA_IA"  = c("strict nature reserve", "strict reserve", "1a", "ia", "Ia"),
    "WDPA_IB"  = c("wilderness area", "wilderness", "1b", "ib", "Ib"),
    "WDPA_II"  = c("national park", "park", "2", "ii", "II"),
    "WDPA_III" = c("natural monument", "monument", "3", "iii", "III"),
    "WDPA_IV"  = c("habitat species management", "habitat management", "4", "iv", "IV"),
    "WDPA_V"   = c("protected landscape", "protected seascape", "landscape", "5", "v", "V"),
    "WDPA_VI"  = c("sustainable use", "natural resources", "6", "vi", "VI"),
    "WDPA_ALL" = c("all", "combined", "full", "total", "all protected areas")
  )
  
  # URL mapping for canonical codes
  url_lookup <- list(
    "WDPA_IA"  = "https://figshare.com/ndownloader/files/59746952?private_link=f0cabc378ea496838f66",
    "WDPA_IB"  = "https://figshare.com/ndownloader/files/59746949?private_link=f0cabc378ea496838f66",
    "WDPA_II"  = "https://figshare.com/ndownloader/files/59746562?private_link=f0cabc378ea496838f66",
    "WDPA_III" = "https://figshare.com/ndownloader/files/59746559?private_link=f0cabc378ea496838f66",
    "WDPA_IV"  = "https://figshare.com/ndownloader/files/59746565?private_link=f0cabc378ea496838f66",
    "WDPA_V"   = "https://figshare.com/ndownloader/files/59746568?private_link=f0cabc378ea496838f66",
    "WDPA_VI"  = "https://figshare.com/ndownloader/files/59746571?private_link=f0cabc378ea496838f66",
    "WDPA_ALL" = "https://figshare.com/ndownloader/files/59747045?private_link=f0cabc378ea496838f66"
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
  for (canon in names(protection_lookup)) {
    for (syn in protection_lookup[[canon]]) {
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
      "Unknown WDPA variables:",
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
  
  cli::cli_alert_info("Starting the download of WDPA data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- url_lookup[[canon]]
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
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