# R/var_get_freshwater.R
#' Download Freshwater environmental variables
#' @noRd
var_get_freshwater <- function(bbox, resolution, variables, temp_dir, ...) {
  # HydroSHEDS and related datasets
  base_url <- "https://data.hydrosheds.org/file/HydroSHEDS/"
  
  downloaded_files <- character()
  
  freshwater_vars <- list(
    "flow_accumulation" = "HydroSHEDS_ACC",
    "flow_direction" = "HydroSHEDS_DIR", 
    "basin" = "HydroSHEDS_BAS",
    "stream_distance" = "HydroSHEDS_DIS",
    "elevation" = "HydroSHEDS_DEM"
  )
  
  for (var in variables) {
    if (var %in% names(freshwater_vars)) {
      # Regional tiles based on bbox
      # This is simplified - actual implementation would need tile selection
      filename <- sprintf("%s_sa_bil.zip", freshwater_vars[[var]])
      url <- paste0(base_url, filename)
      dest_file <- file.path(temp_dir, filename)
      
      if (download_file(url, dest_file)) {
        # Unzip and extract relevant files
        unzip(dest_file, exdir = temp_dir)
        bil_files <- list.files(temp_dir, pattern = "\\.bil$", full.names = TRUE)
        downloaded_files <- c(downloaded_files, bil_files)
      }
    }
  }
  
  return(downloaded_files)
}