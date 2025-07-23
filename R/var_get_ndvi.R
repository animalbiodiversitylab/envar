# R/var_get_ndvi.R
#' Download NDVI time series data
#' @noRd
var_get_ndvi <- function(bbox, resolution, variables, source = "modis", 
                         year = 2022, temp_dir, ...) {
  downloaded_files <- character()
  
  if (source == "modis") {
    # MODIS MOD13A3 monthly NDVI
    base_url <- "https://e4ftl01.cr.usgs.gov/MOLT/MOD13A3.061/"
    
    # Monthly NDVI
    if ("ndvi_monthly" %in% variables) {
      for (month in 1:12) {
        # Would need proper MODIS tile selection based on bbox
        # Simplified version:
        date_str <- sprintf("%d.%02d.01", year, month)
        filename <- sprintf("MOD13A3.A%d%03d.h10v08.061.*.hdf", 
                            year, month * 30)  # Approximate DOY
        
        url <- paste0(base_url, date_str, "/", filename)
        dest_file <- file.path(temp_dir, sprintf("ndvi_%d_%02d.hdf", year, month))
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  } else if (source == "gimms") {
    # GIMMS NDVI3g
    base_url <- "https://ecocast.arc.nasa.gov/data/pub/gimms/3g.v1/"
    
    if ("ndvi_annual" %in% variables) {
      filename <- sprintf("ndvi3g_geo_v1_%d_1km.tif", year)
      url <- paste0(base_url, filename)
      dest_file <- file.path(temp_dir, filename)
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  # Derived NDVI metrics
  ndvi_metrics <- c("ndvi_mean", "ndvi_max", "ndvi_min", "ndvi_std", 
                    "ndvi_amplitude", "greenup_date", "senescence_date")
  
  for (metric in ndvi_metrics) {
    if (metric %in% variables) {
      # These would be calculated from monthly data
      # or downloaded from processed datasets
      filename <- sprintf("%s_%d.tif", metric, year)
      dest_file <- file.path(temp_dir, filename)
      # Process or download pre-calculated metrics
    }
  }
  
  return(downloaded_files)
}