# R/var_get_chelsa_cmip5.R
#' Download CHELSA CMIP5 time series data
#' @noRd
var_get_chelsa_cmip5 <- function(bbox, resolution, variables, model = "ACCESS1-3", 
                                 scenario = "rcp85", period = "2061-2080", 
                                 temp_dir, ...) {
  base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies"
  
  downloaded_files <- character()
  
  # Map variables
  var_map <- list(
    "tas" = "tas",
    "tasmin" = "tasmin", 
    "tasmax" = "tasmax",
    "pr" = "pr"
  )
  
  for (var in variables) {
    if (!var %in% names(var_map)) {
      cli::cli_alert_warning("Variable {.val {var}} not available for CHELSA CMIP5")
      next
    }
    
    chelsa_var <- var_map[[var]]
    
    # Monthly data for future projections
    for (month in 1:12) {
      url <- sprintf("%s/%s/%s/%s/CHELSA_%s_%02d_%s_%s_V.2.1.tif",
                     base_url, period, model, scenario, 
                     chelsa_var, month, model, scenario)
      dest_file <- file.path(temp_dir, basename(url))
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}