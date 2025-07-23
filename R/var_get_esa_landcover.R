# R/var_get_esa_landcover.R
#' Download ESA Land Cover CCI data
#' @noRd
var_get_esa_landcover <- function(bbox, resolution, variables, year = 2020, temp_dir, ...) {
  # ESA CCI Land Cover
  base_url <- "https://maps.elie.ucl.ac.be/CCI/viewer/download/"
  
  downloaded_files <- character()
  
  if ("landcover" %in% variables) {
    # Annual land cover maps
    filename <- sprintf("ESACCI-LC-L4-LCCS-Map-300m-P1Y-%d-v2.1.1.tif", year)
    url <- paste0(base_url, filename)
    dest_file <- file.path(temp_dir, filename)
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  # Land cover fractions
  lc_classes <- c("cropland", "forest", "grassland", "shrubland", "wetland", 
                  "urban", "bare", "water", "snow")
  
  for (class in lc_classes) {
    if (class %in% variables) {
      filename <- sprintf("ESACCI-LC-L4-%s-Cond-%d-v2.1.1.tif", class, year)
      url <- paste0(base_url, filename)
      dest_file <- file.path(temp_dir, filename)
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}