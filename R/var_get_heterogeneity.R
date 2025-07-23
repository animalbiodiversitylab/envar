# R/var_get_heterogeneity.R
#' Download heterogeneity metrics from various spectral indices
#' @noRd
var_get_heterogeneity <- function(bbox, resolution, variables, indices = "ndvi", temp_dir, ...) {
  # Spectral heterogeneity from MODIS
  base_url <- "https://data.earthenv.org/heterogeneity/"
  
  downloaded_files <- character()
  
  # Available heterogeneity metrics
  metrics <- c("cv", "evenness", "range", "shannon", "simpson", "std", 
               "contrast", "correlation", "dissimilarity", "entropy", 
               "homogeneity", "maximum", "uniformity", "variance")
  
  # Available spectral indices
  valid_indices <- c("ndvi", "evi", "red", "nir", "blue", "green", "swir1", "swir2")
  
  # Validate indices parameter
  indices <- intersect(indices, valid_indices)
  if (length(indices) == 0) {
    cli::cli_alert_warning("No valid indices specified. Using NDVI as default.")
    indices <- "ndvi"
  }
  
  for (var in variables) {
    if (var %in% metrics) {
      for (idx in indices) {
        filename <- sprintf("%s_%s_1km.tif", var, idx)
        url <- paste0(base_url, filename)
        dest_file <- file.path(temp_dir, filename)
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  }
  
  # Additional heterogeneity products
  if ("texture_metrics" %in% variables) {
    # GLCM texture metrics
    texture_vars <- c("contrast", "dissimilarity", "homogeneity", "energy", "correlation")
    for (tex in texture_vars) {
      for (idx in indices) {
        filename <- sprintf("glcm_%s_%s_1km.tif", tex, idx)
        url <- paste0(base_url, "texture/", filename)
        dest_file <- file.path(temp_dir, filename)
        
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
  }
  
  return(downloaded_files)
}