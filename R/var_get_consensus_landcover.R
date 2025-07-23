# R/var_get_consensus_landcover.R
#' Download Consensus Land Cover data
#' @noRd
var_get_consensus_landcover <- function(bbox, resolution, variables, temp_dir, ...) {
  # Based on Tuanmu & Jetz 2014
  base_url <- "https://data.earthenv.org/consensus_landcover/"
  
  downloaded_files <- character()
  
  # 12 land cover classes
  lc_classes <- list(
    "evergreen_needleleaf" = "Consensus_full_class_1.tif",
    "evergreen_broadleaf" = "Consensus_full_class_2.tif",
    "deciduous_needleleaf" = "Consensus_full_class_3.tif",
    "deciduous_broadleaf" = "Consensus_full_class_4.tif",
    "mixed_forest" = "Consensus_full_class_5.tif",
    "shrublands" = "Consensus_full_class_6.tif",
    "herbaceous" = "Consensus_full_class_7.tif",
    "cultivated" = "Consensus_full_class_8.tif",
    "urban" = "Consensus_full_class_9.tif",
    "barren" = "Consensus_full_class_10.tif",
    "water" = "Consensus_full_class_11.tif",
    "snow_ice" = "Consensus_full_class_12.tif"
  )
  
  for (var in variables) {
    if (var %in% names(lc_classes)) {
      filename <- lc_classes[[var]]
      url <- paste0(base_url, filename)
      dest_file <- file.path(temp_dir, filename)
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}