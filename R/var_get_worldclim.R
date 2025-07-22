# R/var_get_worldclim.R
#' Download WorldClim data
#' @noRd
var_get_worldclim <- function(bbox, resolution, variables, temp_dir, ...) {
  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1"
  
  # Resolution mapping
  res_map <- list(
    "30s" = "30s",
    "1km" = "30s",  # Will resample later
    "2.5m" = "2.5m",
    "5m" = "5m",
    "10m" = "10m"
  )
  
  wc_res <- res_map[[resolution]]
  downloaded_files <- character()
  
  for (var in variables) {
    if (var == "bioclim") {
      # Download all 19 bioclim variables
      for (i in 1:19) {
        url <- sprintf("%s/%s/wc2.1_%s_bio_%d.tif", 
                       base_url, wc_res, wc_res, i)
        dest_file <- file.path(temp_dir, basename(url))
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    } else {
      # Download monthly variables
      for (month in 1:12) {
        url <- sprintf("%s/%s/wc2.1_%s_%s_%02d.tif",
                       base_url, wc_res, wc_res, var, month)
        dest_file <- file.path(temp_dir, basename(url))
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  }
  
  return(downloaded_files)
}