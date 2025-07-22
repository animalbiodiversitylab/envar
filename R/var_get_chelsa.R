# R/var_get_chelsa.R
#' Download CHELSA data
#' @noRd
var_get_chelsa <- function(bbox, resolution, variables, temp_dir, ...) {
  base_url <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010"
  
  downloaded_files <- character()
  
  for (var in variables) {
    if (var == "bioclim") {
      # Download CHELSA bioclim variables
      for (i in 1:19) {
        url <- sprintf("%s/bio/CHELSA_bio%d_1981-2010_V.2.1.tif", 
                       base_url, i)
        dest_file <- file.path(temp_dir, basename(url))
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    } else {
      # Map variable names
      chelsa_var <- switch(var,
                           "tas" = "tas",
                           "tasmin" = "tasmin",
                           "tasmax" = "tasmax",
                           "pr" = "pr"
      )
      
      # Download monthly data
      for (month in 1:12) {
        url <- sprintf("%s/%s/CHELSA_%s_%02d_1981-2010_V.2.1.tif",
                       base_url, chelsa_var, chelsa_var, month)
        dest_file <- file.path(temp_dir, basename(url))
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  }
  
  return(downloaded_files)
}