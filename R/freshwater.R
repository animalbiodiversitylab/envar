# R/freshwater.R

#' Download and process EarthEnv Freshwater Environmental Variables
#'
#' `freshwater_variables()` downloads, processes, and extracts variables from the
#' **Near-global freshwater-specific environmental variables** dataset.
#' These variables are available at a 1-km resolution and capture upstream
#' catchment characteristics, including topography, land cover, soil, and climate.
#'
#' The function allows users to input either:
#' - **canonical filenames**, e.g. `"monthly_tmin_average.nc"`
#' - **human-readable names**, e.g. `"min temp"`, `"upstream precipitation"`,
#'   `"slope"`, `"soil average"`, etc.
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
#' **Domisch, S., Amatulli, G. & Jetz, W. (2015).**
#' *Near-global freshwater-specific environmental variables for biodiversity
#' analyses in 1 km resolution.* Scientific Data, **2**, 150073.
#' https://doi.org/10.1038/sdata.2015.73
#'
#'
#' ## Available variables
#'
#' | Human-readable name (examples) | Canonical filename (code)               |
#' |--------------------------------|-----------------------------------------|
#' | min temp (average)             | monthly_tmin_average.nc                 |
#' | max temp (average)             | monthly_tmax_average.nc                 |
#' | precipitation (sum)            | monthly_prec_sum.nc                     |
#' | min temp (weighted)            | monthly_tmin_weighted_average.nc        |
#' | max temp (weighted)            | monthly_tmax_weighted_average.nc        |
#' | precipitation (weighted)       | monthly_prec_weighted_sum.nc            |
#' | hydroclim (avg/sum)            | hydroclim_average+sum.nc                |
#' | hydroclim (weighted)           | hydroclim_weighted_average+sum.nc       |
#' | elevation                      | elevation.nc                            |
#' | slope                          | slope.nc                                |
#' | flow accumulation              | flow_acc.nc                             |
#' | landcover (min)                | landcover_minimum.nc                    |
#' | landcover (max)                | landcover_maximum.nc                    |
#' | landcover (range)              | landcover_range.nc                      |
#' | landcover (avg)                | landcover_average.nc                    |
#' | landcover (weighted)           | landcover_weighted_average.nc           |
#' | geology (weighted)             | geology_weighted_sum.nc                 |
#' | soil (min)                     | soil_minimum.nc                         |
#' | soil (max)                     | soil_maximum.nc                         |
#' | soil (range)                   | soil_range.nc                           |
#' | soil (avg)                     | soil_average.nc                         |
#' | soil (weighted)                | soil_weighted_average.nc                |
#' | quality control                | quality_control.nc                      |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical filenames or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed freshwater layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

freshwater <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Freshwater Environmental Variables.\n",
    "Citation: Domisch, S., Amatulli, G. & Jetz, W. (2015). Near-global freshwater-specific environmental variables for biodiversity analyses in 1 km resolution. Scientific Data.\n",
    "DOI: {.url https://doi.org/10.1038/sdata.2015.73}\n",
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
  freshwater_lookup <- list(
    "monthly_tmin_average.nc"           = c("monthly minimum temperature average", "min temp average", "tmin avg", "tmin"),
    "monthly_tmax_average.nc"           = c("monthly maximum temperature average", "max temp average", "tmax avg", "tmax"),
    "monthly_prec_sum.nc"               = c("monthly upstream precipitation sum", "precipitation sum", "precip sum", "prec"),
    "monthly_tmin_weighted_average.nc"  = c("monthly minimum temperature weighted", "min temp weighted", "tmin weighted"),
    "monthly_tmax_weighted_average.nc"  = c("monthly maximum temperature weighted", "max temp weighted", "tmax weighted"),
    "monthly_prec_weighted_sum.nc"      = c("monthly upstream precipitation weighted", "precipitation weighted", "precip weighted"),
    "hydroclim_average+sum.nc"          = c("hydroclimatic variables average", "hydroclim average", "hydroclim"),
    "hydroclim_weighted_average+sum.nc" = c("hydroclimatic variables weighted", "hydroclim weighted"),
    "elevation.nc"                      = c("upstream elevation", "elevation", "dem"),
    "slope.nc"                          = c("upstream slope", "slope"),
    "flow_acc.nc"                       = c("stream length", "flow accumulation", "flow"),
    "landcover_minimum.nc"              = c("upstream landcover minimum", "landcover min"),
    "landcover_maximum.nc"              = c("upstream landcover maximum", "landcover max"),
    "landcover_range.nc"                = c("upstream landcover range", "landcover range"),
    "landcover_average.nc"              = c("upstream landcover average", "landcover avg", "landcover"),
    "landcover_weighted_average.nc"     = c("upstream landcover weighted", "landcover weighted"),
    "geology_weighted_sum.nc"           = c("upstream geology", "geology weighted", "geology"),
    "soil_minimum.nc"                   = c("upstream soil minimum", "soil min"),
    "soil_maximum.nc"                   = c("upstream soil maximum", "soil max"),
    "soil_range.nc"                     = c("upstream soil range", "soil range"),
    "soil_average.nc"                   = c("upstream soil average", "soil avg", "soil"),
    "soil_weighted_average.nc"          = c("upstream soil weighted", "soil weighted"),
    "quality_control.nc"                = c("quality control", "qc")
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
  for (canon in names(freshwater_lookup)) {
    for (syn in freshwater_lookup[[canon]]) {
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
      "Unknown Freshwater variables:",
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
  base_url <- "https://data.earthenv.org/streams"
  
  cli::cli_alert_info("Starting the download of Freshwater data...")
  
  for (canon in requested_codes) {
    # In this function, 'canon' is the full filename (e.g. monthly_tmin.nc)
    filename <- canon 
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