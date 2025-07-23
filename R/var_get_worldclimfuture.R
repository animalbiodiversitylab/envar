#' Download future climate data from WorldClim (CMIP6)
#'
#' This function downloads future climate projection data from the WorldClim
#' database based on the specified GCM, SSP, time period, resolution, and variables.
#'
#' @param bbox A bounding box object (not directly used in URL construction but
#' required for compatibility with the general var_get structure).
#' @param resolution The spatial resolution in arc-seconds.
#' Valid options: 30, 150 (2.5m), 300 (5m), 600 (10m).
#' @param variables A character vector of variables to download.
#' Valid options: "tmin", "tmax", "prec", "bioc".
#' @param temp_dir The temporary directory to store downloaded files.
#' @param gcm (character) The General Circulation Model. E.g., "ACCESS-CM2".
#' @param ssp (character) The Shared Socioeconomic Pathway. E.g., "ssp126", "ssp585".
#' @param time_period (character) The future time period. E.g., "2021-2040".
#'
#' @return A character vector of paths to the downloaded files.
#' @noRd
var_get_worldclimfuture <- function(bbox,
                                    resolution,
                                    variables,
                                    temp_dir,
                                    gcm,
                                    ssp,
                                    time_period) {
  
  # Non c'è più bisogno di estrarli da '...'
  if (is.null(gcm) || is.null(ssp) || is.null(time_period)) {
    stop("Required arguments 'gcm', 'ssp', and 'time_period' must be provided.")
  }
  
  # Mapping per la risoluzione da numerico a stringa (invariato)
  resolution_map <- list(
    "30" = "30s",
    "150" = "2.5m",
    "300" = "5m",
    "600" = "10m",
    "1km" = "30s"
  )
  resolution_str <- resolution_map[[as.character(resolution)]]
  if (is.null(resolution_str)) {
    stop(paste("Resolution", resolution, "is not supported. Valid options are:",
               paste(names(resolution_map), collapse = ", ")))
  }
  
  # Validazione delle variabili (invariato)
  valid_variables <- c("tmin", "tmax", "prec", "bioclim")
  
  if (!all(variables %in% valid_variables)) {
    stop(paste("Invalid variable provided. Valid options are:",
               paste(valid_variables, collapse = ", ")))
  }
  

  # --- Costruzione dell'URL e Download (invariato) ---
  base_url <- "https://geodata.ucdavis.edu/cmip6/"
  downloaded_files <- character()
  
  for (variable in variables) {

    if (variable=="bioclim"){variable<-"bioc"}
    
    file_name <- sprintf("wc2.1_%s_%s_%s_%s_%s",
                         resolution_str,
                         variable,
                         gcm,
                         ssp,
                         time_period)
    
    url <- paste0(base_url, resolution_str, "/", gcm, "/", ssp, "/", file_name, ".tif")
    dest_file <- file.path(temp_dir, file_name)
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  return(downloaded_files)
}
