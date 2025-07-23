# R/var_get_consensus_landcover.R 
#' Download Consensus Land Cover data 
#' @param discover A boolean to select the version with DISCover. Defaults to FALSE. 
#' @noRd 
var_get_consensus_landcover <- function(bbox, resolution, variables, temp_dir, discover = FALSE, ...) { 
  
  if (discover) { 
    # The full version integrates GlobCover, MODIS, GLC2000, and DISCover. [2] 
    cli::cli_alert_info("Using Consensus Land Cover (with DISCover)") 
    base_url <- "https://data.earthenv.org/consensus_landcover/with_DISCover/" 
    lc_classes <- list( 
      "evergreen_needleleaf" = "consensus_full_class_1.tif", 
      "evergreen_broadleaf" = "consensus_full_class_2.tif", 
      "deciduous_broadleaf" = "consensus_full_class_3.tif", 
      "mixed_trees" = "consensus_full_class_4.tif", 
      "shrubs" = "consensus_full_class_5.tif", 
      "herbaceous" = "consensus_full_class_6.tif", 
      "cultivated" = "consensus_full_class_7.tif", 
      "flooded_vegetation" = "consensus_full_class_8.tif", 
      "urban" = "consensus_full_class_9.tif", 
      "snow_ice" = "consensus_full_class_10.tif", 
      "barren" = "consensus_full_class_11.tif", 
      "water" = "consensus_full_class_12.tif" 
    ) 
  } else { 
    # The reduced version integrates GlobCover, MODIS, and GLC2000. [2] 
    cli::cli_alert_info("Using Consensus Land Cover (without DISCover)") 
    base_url <- "https://data.earthenv.org/consensus_landcover/without_DISCover/" 
    lc_classes <- list( 
      "evergreen_needleleaf" = "Consensus_reduced_class_1.tif", 
      "evergreen_broadleaf" = "Consensus_reduced_class_2.tif", 
      "deciduous_broadleaf" = "Consensus_reduced_class_3.tif", 
      "mixed_trees" = "Consensus_reduced_class_4.tif", 
      "shrubs" = "Consensus_reduced_class_5.tif", 
      "herbaceous" = "Consensus_reduced_class_6.tif", 
      "cultivated" = "Consensus_reduced_class_7.tif", 
      "flooded_vegetation" = "Consensus_reduced_class_8.tif", 
      "urban" = "Consensus_reduced_class_9.tif", 
      "snow_ice" = "Consensus_reduced_class_10.tif", 
      "barren" = "Consensus_reduced_class_11.tif", 
      "water" = "Consensus_reduced_class_12.tif" 
    ) 
  } 
  
  downloaded_files <- character() 
  
  for (var in variables) { 
    if (var %in% names(lc_classes)) { 
      filename <- lc_classes[[var]] 
      url <- paste0(base_url, filename) 
      dest_file <- file.path(temp_dir, filename) 
      
      if (download_file(url, dest_file)) { 
        downloaded_files <- c(downloaded_files, dest_file) 
      } 
    } else { 
      cli::cli_warn("Variable {.val {var}} is not available for Consensus Land Cover.") 
    } 
  } 
  
  if (length(downloaded_files) == 0) { 
    cli::cli_abort("No valid variables were specified for Consensus Land Cover.") 
  } 
  
  return(downloaded_files) 
} 