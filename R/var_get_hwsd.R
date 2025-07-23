# R/var_get_hwsd.R
#' Download Harmonized World Soil Database
#' @noRd
var_get_hwsd <- function(bbox, resolution, variables, temp_dir, ...) {
  # HWSD v1.2 from FAO
  base_url <- "https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/"
  
  downloaded_files <- character()
  
  soil_vars <- c("sand", "silt", "clay", "gravel", "bulk_density", "organic_carbon",
                 "ph", "cec", "bs", "sodicity", "salinity", "texture_class")
  
  if (any(soil_vars %in% variables)) {
    # Download main HWSD file
    hwsd_url <- "https://data.isric.org/geoserver/hwsd/wms"
    
    # This would need proper WMS handling
    # Simplified version:
    for (var in variables[variables %in% soil_vars]) {
      filename <- sprintf("HWSD_%s.tif", var)
      dest_file <- file.path(temp_dir, filename)
      
      # In reality, would need to construct proper WMS request
      # based on bbox and variable
      wms_url <- sprintf("%s?request=GetMap&layers=%s&bbox=%s&format=image/geotiff",
                         hwsd_url, var, paste(bbox, collapse = ","))
      
      if (download_file(wms_url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}