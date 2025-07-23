# R/var_get_freshwater.R
#' Download Freshwater environmental variables from EarthEnv
#'
#' Downloads freshwater environmental variables as global NetCDF (.nc) files
#' from the EarthEnv streams dataset.
#'
#' @param variables A character vector of variable names to download.
#' @param temp_dir A string specifying the directory where files will be saved.
#' @param ... Additional arguments (not used in this function but kept for consistency).
#' @return A character vector with the full paths to the downloaded files.
#' @noRd
var_get_freshwater <- function(variables, temp_dir, ...) {
  # Base URL for EarthEnv streams data
  base_url <- "https://data.earthenv.org/streams/"
  
  # A named list to map user-friendly variable names to their filenames
  freshwater_vars <- list(
    # Climate variables
    "tmin_monthly_avg" = "monthly_tmin_average.nc",
    "tmax_monthly_avg" = "monthly_tmax_average.nc",
    "prec_monthly_sum" = "monthly_prec_sum.nc",
    "tmin_monthly_weighted_avg" = "monthly_tmin_weighted_average.nc",
    "tmax_monthly_weighted_avg" = "monthly_tmax_weighted_average.nc",
    "prec_monthly_weighted_sum" = "monthly_prec_weighted_sum.nc",
    
    # Hydro-climate variables
    "hydroclim_avg_sum" = "hydroclim_average+sum.nc",
    "hydroclim_weighted_avg_sum" = "hydroclim_weighted_average+sum.nc",
    
    # Topographic variables
    "elevation" = "elevation.nc",
    "slope" = "slope.nc",
    "flow_accumulation" = "flow_acc.nc",
    
    # Landcover variables
    "landcover_min" = "landcover_minimum.nc",
    "landcover_max" = "landcover_maximum.nc",
    "landcover_range" = "landcover_range.nc",
    "landcover_avg" = "landcover_average.nc",
    "landcover_weighted_avg" = "landcover_weighted_average.nc",
    
    # Geology variables
    "geology_weighted_sum" = "geology_weighted_sum.nc",
    
    # Soil variables
    "soil_min" = "soil_minimum.nc",
    "soil_max" = "soil_maximum.nc",
    "soil_range" = "soil_range.nc",
    "soil_avg" = "soil_average.nc",
    "soil_weighted_avg" = "soil_weighted_average.nc",
    
    # Quality control
    "quality_control" = "quality_control.nc"
  )
  
  # Progress tracking setup
  cli::cli_h2("Downloading freshwater environmental variables")
  valid_vars <- variables[variables %in% names(freshwater_vars)]
  invalid_vars <- variables[!variables %in% names(freshwater_vars)]
  
  # Report invalid variables
  if (length(invalid_vars) > 0) {
    cli::cli_alert_warning("Skipping unrecognized variables: {.val {invalid_vars}}")
  }
  
  if (length(valid_vars) == 0) {
    cli::cli_alert_danger("No valid variables to download")
    return(character())
  }
  
  cli::cli_alert_info("Found {length(valid_vars)} variable{?s} to download")
  
  downloaded_files <- character()
  
  # Download each variable with progress tracking
  for (i in seq_along(valid_vars)) {
    var <- valid_vars[i]
    filename <- freshwater_vars[[var]]
    url <- paste0(base_url, filename)
    dest_file <- file.path(temp_dir, filename)
    
    # Progress step for current download
    cli::cli_progress_step("Downloading {.val {var}} ({i}/{length(valid_vars)})")
    
    # Check if file already exists to avoid re-downloading
    if (file.exists(dest_file)) {
      cli::cli_alert_info("File {.file {filename}} already exists, skipping download")
      downloaded_files <- c(downloaded_files, dest_file)
      next # Move to the next variable
    }
    
    # Use the robust download_file function that handles retries, timeouts, and progress
    # Pass '...' to the download function for additional arguments
    if (download_file(url, dest_file, ...)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  # Summary of downloads
  if (length(downloaded_files) > 0) {
    cli::cli_alert_success("Successfully downloaded {length(downloaded_files)} file{?s}")
  } else {
    cli::cli_alert_warning("No files were downloaded")
  }
  
  # downloaded_files <- terra::rast(downloaded_files)
  return(downloaded_files)
}
