# R/var_get_climate_stability.R
#' Download Climate Stability Index
#' @noRd
var_get_climate_stability <- function(bbox, resolution, variables, temp_dir, ...) {
  # Based on Iwamura et al. 2013
  base_url <- "https://datadryad.org/stash/downloads/file_stream/"
  
  downloaded_files <- character()
  
  stability_vars <- list(
    "temperature_stability" = "12345", # Example file ID
    "precipitation_stability" = "12346"
  )
  
  for (var in variables) {
    if (var %in% names(stability_vars)) {
      file_id <- stability_vars[[var]]
      url <- paste0(base_url, file_id)
      dest_file <- file.path(temp_dir, paste0(var, ".tif"))
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}