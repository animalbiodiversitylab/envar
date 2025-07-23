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
  
  downloaded_files <- character()
  
  for (var in variables) {
    if (var %in% names(freshwater_vars)) {
      # Get the filename from our mapping list
      filename <- freshwater_vars[[var]]
      url <- paste0(base_url, filename)
      dest_file <- file.path(temp_dir, filename)
      
      # Check if file already exists to avoid re-downloading
      if (file.exists(dest_file)) {
        message(sprintf("File '%s' already exists in temp_dir. Skipping download.", filename))
        downloaded_files <- c(downloaded_files, dest_file)
        next # Move to the next variable
      }
      
      # Using R's built-in download.file function
      # The original function used a custom 'download_file', this is a standard equivalent.
      tryCatch({
        message(sprintf("Downloading '%s'...", filename))
        download.file(url, dest_file, mode = "wb", quiet = TRUE)
        downloaded_files <- c(downloaded_files, dest_file)
        message("Download complete.")
      }, error = function(e) {
        warning(sprintf("Failed to download %s. Error: %s", url, e$message))
      })
      
    } else {
      warning(sprintf("Variable '%s' is not recognized. Check available names. Skipping.", var))
    }
  }
  
  # The function no longer needs bbox or resolution for the download part,
  # as these are global files. These arguments might be used later for cropping/processing.
  
  return(downloaded_files)
}