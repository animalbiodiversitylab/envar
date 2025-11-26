# R/earthenvlandcover.R

#' Download and process EarthEnv Consensus Land Cover layers
#'
#' `earthenvlandcover()` downloads, processes, and extracts variables from the
#' **EarthEnv Consensus Land Cover** dataset. Each variable corresponds to a 
#' global Cloud-Optimized GeoTIFF (COG) representing the consensus prevalence 
#' of a specific land cover class.
#'
#' The function allows users to input either:
#' - **canonical EarthEnv filenames**, e.g. `"consensus_full_class_1"`, `"consensus_full_class_9"`
#' - **human-readable names**, e.g. `"needleleaf trees"`, `"urban"`,
#'   `"open water"`, `"cultivated"`, etc.
#'
#' It automatically:
#' - downloads the selected files,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#' ## Citation
#' If you use EarthEnv Consensus Land Cover data, please cite:
#'
#' **Tuanmu, M.-N. and W. Jetz. (2014).**
#' *A global 1-km consensus land-cover product for biodiversity and ecosystem modeling.*
#' Global Ecology and Biogeography 23(9): 1031-1045.
#' https://doi.org/10.1111/geb.12182
#'
#' _Note: Users should verify the terms of use for EarthEnv data provided 
#' at https://www.earthenv.org/._
#'
#' ## Available variables
#'
#' | Human-readable name                 | Canonical variable code      |
#' |-------------------------------------|------------------------------|
#' | Evergreen/Deciduous Needleleaf Trees| consensus_full_class_1       |
#' | Evergreen Broadleaf Trees           | consensus_full_class_2       |
#' | Deciduous Broadleaf Trees           | consensus_full_class_3       |
#' | Mixed/Other Trees                   | consensus_full_class_4       |
#' | Shrubs                              | consensus_full_class_5       |
#' | Herbaceous Vegetation               | consensus_full_class_6       |
#' | Cultivated and Managed Vegetation   | consensus_full_class_7       |
#' | Regularly Flooded Vegetation        | consensus_full_class_8       |
#' | Urban/Built-up                      | consensus_full_class_9       |
#' | Snow/Ice                            | consensus_full_class_10      |
#' | Barren                              | consensus_full_class_11      |
#' | Open Water                          | consensus_full_class_12      |
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param discover Logical. If `TRUE` (default), downloads the version integrated 
#'        with the DISCover dataset. If `FALSE`, downloads the version without 
#'        DISCover integration.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed EarthEnv layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
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
    if (inherits(x, "SpatRaster")) processed_stack <- c(x, processed_stack)
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}