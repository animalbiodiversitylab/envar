# R/var_get_topography.R
#' Download Topography data from various sources
#' @noRd
var_get_topography <- function(bbox, resolution, variables, source = "gmted2010", temp_dir, ...) {
  downloaded_files <- character()
  
  if (source == "gmted2010") {
    # GMTED2010 from EarthEnv
    base_url <- "https://data.earthenv.org/topography/"
    
    topo_vars <- list(
      "elevation" = "elevation_1KMmd_GMTEDmd.tif",
      "slope" = "slope_1KMmd_GMTEDmd.tif",
      "aspect" = "aspect_1KMmd_GMTEDmd.tif",
      "roughness" = "roughness_1KMmd_GMTEDmd.tif",
      "tri" = "tri_1KMmd_GMTEDmd.tif",  # Terrain Ruggedness Index
      "tpi" = "tpi_1KMmd_GMTEDmd.tif"   # Topographic Position Index
    )
    
    for (var in variables) {
      if (var %in% names(topo_vars)) {
        url <- paste0(base_url, topo_vars[[var]])
        dest_file <- file.path(temp_dir, topo_vars[[var]])
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  } else if (source == "srtm") {
    # SRTM 90m data
    # Implementation for SRTM tiles based on bbox
    cli::cli_alert_info("SRTM download implementation needed")
  }
  
  return(downloaded_files)
}