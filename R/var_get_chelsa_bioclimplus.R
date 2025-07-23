# R/var_get_chelsa_bioclimplus.R 
#' Download CHELSA-Bioclim+ extended bioclimatic variables
#' @noRd
var_get_chelsa_bioclimplus <- function(bbox, resolution, variables, temp_dir, ...) {
  base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio"
  
  downloaded_files <- character()
  
  # Extended bioclim variables (bio20-bio42)
  if ("bioclimplus" %in% variables) {
    for (i in 20:42) {
      url <- sprintf("%s/CHELSA_bio%d_1981-2010_V.2.1.tif", base_url, i)
      dest_file <- file.path(temp_dir, basename(url))
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  # Growing degree days, frost days, etc.
  additional_vars <- c("gdd0", "gdd5", "gdd10", "nfd", "scd", "lgd")
  for (var in additional_vars) {
    if (var %in% variables) {
      url <- sprintf("%s/CHELSA_%s_1981-2010_V.2.1.tif", base_url, var)
      dest_file <- file.path(temp_dir, basename(url))
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}