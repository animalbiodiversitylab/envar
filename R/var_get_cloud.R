# R/var_get_cloud.R
#' Download Cloud Cover data from EarthEnv
#' @noRd
var_get_cloud <- function(bbox, resolution, variables, temp_dir, ...) {
  base_url <- "https://data.earthenv.org/cloud/"
  downloaded_files <- character()
  
  # Annual mean cloud cover
  if ("cloud_annual" %in% variables) {
    url <- paste0(base_url, "MODCF_meanannual.tif")
    dest_file <- file.path(temp_dir, "cloud_mean_annual.tif")
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  # Monthly cloud cover
  if ("cloud_monthly" %in% variables) {
    for (month in 1:12) {
      url <- sprintf("%sMODCF_monthlymean_%02d.tif", base_url, month)
      dest_file <- file.path(temp_dir, sprintf("cloud_month_%02d.tif", month))
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  # Intra-annual variability
  if ("cloud_variability" %in% variables) {
    url <- paste0(base_url, "MODCF_intraannualSD.tif")
    dest_file <- file.path(temp_dir, "cloud_intraannual_sd.tif")
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  # Inter-annual variability  
  if ("cloud_interannual" %in% variables) {
    url <- paste0(base_url, "MODCF_interannualSD.tif")
    dest_file <- file.path(temp_dir, "cloud_interannual_sd.tif")
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  return(downloaded_files)
}