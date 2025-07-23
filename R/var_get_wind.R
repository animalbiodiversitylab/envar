# R/var_get_wind.R
#' Download Global Wind Atlas data
#' @noRd
var_get_wind <- function(bbox, resolution, variables, height = "50", temp_dir, ...) {
  # Global Wind Atlas API
  base_url <- "https://globalwindatlas.info/api/gis/country/"
  
  downloaded_files <- character()
  
  wind_vars <- c("wind_speed", "power_density", "capacity_factor")
  
  for (var in variables) {
    if (var %in% wind_vars) {
      # This would need proper API authentication and requests
      # Simplified version:
      filename <- sprintf("GWA_%s_%sm.tif", var, height)
      dest_file <- file.path(temp_dir, filename)
      
      # Would need to construct proper API request based on bbox
      api_url <- sprintf("%s/download?var=%s&height=%s&bbox=%s",
                         base_url, var, height, paste(bbox, collapse = ","))
      
      if (download_file(api_url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}