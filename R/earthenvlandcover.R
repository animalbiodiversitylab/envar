# R/earthenvlandcover.R

#' Download EarthEnv land cover
#'
#' This function downloads, processes, and extracts variables from the
#' EarthEnv Consensus Land Cover dataset. 
#'
#' Available variables (working synonyms in parentheses):
#'
#' 1 - "evergreen deciduous needleleaf trees" ("needleleaf trees", "needleleaf", "conifer")
#' 
#' 2 - "evergreen broadleaf trees" ("evergreen broadleaf", "broadleaf evergreen")
#' 
#' 3 - "deciduous broadleaf trees" ("deciduous broadleaf", "broadleaf deciduous")
#' 
#' 4 - "mixed other trees" ("mixed trees", "other trees", "mixed forest")
#' 
#' 5 - "shrubs" ("shrubland", "shrub")
#' 
#' 6 - "herbaceous vegetation" ("herbaceous", "grassland", "grass", "herbs")
#' 
#' 7 - "cultivated and managed vegetation" ("cultivated", "managed vegetation", "agriculture", "crops", "cropland")
#' 
#' 8 - "regularly flooded vegetation" ("flooded vegetation", "flooded", "wetland")
#' 
#' 9 - "urban built up" ("urban", "built up", "built-up", "artificial surface")
#' 
#' 10 - "snow ice" ("snow", "ice", "glacier", "permafrost")
#' 
#' 11 - "barren" ("barren land", "bare ground", "bare")
#' 
#' 12 - "open water" ("water", "water bodies")
#'
#' Citation:
#'
#' Tuanmu, M.N., Jetz. W. (2014). "A global 1-km consensus land-cover product
#' for biodiversity and ecosystem modeling." Global Ecology and Biogeography 23: 1031-1045.
#' https://doi.org/10.1111/geb.12182
#'
#' Note: Users should verify the terms of use for EarthEnv data provided 
#' at https://www.earthenv.org/
#' 
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param discover Logical. If `TRUE` (default), downloads the version integrated 
#'        with the DISCover dataset. If `FALSE`, downloads the version without 
#'        DISCover integration.
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#'processed <- var_get(country= "Italy", crs=3035) %>% 
#'earthenvlandcover(vars=c("snow ice"))
#'   }
#' @export


earthenvlandcover <- function(x, vars, discover = TRUE, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Consensus Land Cover layers.\n",
    "Citation: Tuanmu, M.N. and W. Jetz. (2014). A global 1-km consensus land-cover product for biodiversity and ecosystem modeling. Global Ecology and Biogeography.\n",
    "DOI: {.url https://doi.org/10.1111/geb.12182}\n"
  ))
  
  par_list <- get_par(x)
  
  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res <- par_list$res
    crs <- par_list$crs
    is_global <- isTRUE(par_list$is_global)
    is_raster_input <- TRUE
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs <- par_list$crs
    is_raster_input <- FALSE
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  earthenv_lookup <- list(
    "consensus_full_class_1"  = c("evergreen deciduous needleleaf trees", "needleleaf trees", "needleleaf", "conifer"),
    "consensus_full_class_2"  = c("evergreen broadleaf trees", "evergreen broadleaf", "broadleaf evergreen"),
    "consensus_full_class_3"  = c("deciduous broadleaf trees", "deciduous broadleaf", "broadleaf deciduous"),
    "consensus_full_class_4"  = c("mixed other trees", "mixed trees", "other trees", "mixed forest"),
    "consensus_full_class_5"  = c("shrubs", "shrubland", "shrub"),
    "consensus_full_class_6"  = c("herbaceous vegetation", "herbaceous", "grassland", "grass", "herbs"),
    "consensus_full_class_7"  = c("cultivated and managed vegetation", "cultivated", "managed vegetation", "agriculture", "crops", "cropland"),
    "consensus_full_class_8"  = c("regularly flooded vegetation", "flooded vegetation", "flooded", "wetland"),
    "consensus_full_class_9"  = c("urban built up", "urban", "built up", "built-up", "artificial surface"),
    "consensus_full_class_10" = c("snow ice", "snow", "ice", "glacier", "permafrost"),
    "consensus_full_class_11" = c("barren", "barren land", "bare ground", "bare"),
    "consensus_full_class_12" = c("open water", "water", "water bodies")
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
  for (canon in names(earthenv_lookup)) {
    for (syn in earthenv_lookup[[canon]]) {
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
      "Unknown EarthEnv variables:",
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
        if (!is_global) {
          fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {basename(dest_file)}}...")
      
      # Process layer based on whether we're doing global or regional processing
      layer1 <- process_raster_layer(
        layer = layer,
        grid = grid,
        mask = mask,
        res = res,
        crs = crs,
        is_global = is_global
      )
      
      # Assign name to layer
      names(layer1) <- var
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {basename(dest_file)}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) {
        fs::file_delete(dest_file)
      }
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {basename(dest_file)}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {basename(dest_file)}}.")
        if (!is_global) {
          fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
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
      if (!is_global) {
        fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  if (discover) {
    base_url <- "https://data.earthenv.org/consensus_landcover/with_DISCover"
  } else {
    base_url <- "https://data.earthenv.org/consensus_landcover/without_DISCover"
  }
  
  cli::cli_alert_info("Starting the download of EarthEnv Consensus Land Cover data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- file.path(base_url, filename)
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
    handle_file(url, dest, canon)
  }
  
  # --------------------------------------------------------------------
  # Return output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    # If x was already a SpatRaster (from previous function), combine
    if (inherits(x, "SpatRaster")) {
      processed_stack <- c(x, processed_stack)
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}