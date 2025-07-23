# R/var_get_aridity.R
#' Download Global Aridity Index and PET
#' @noRd
var_get_aridity <- function(bbox, resolution, variables, temp_dir, ...) {
  # CGIAR-CSI Global Aridity and PET Database
  base_url <- "https://figshare.com/ndownloader/files/"
  
  downloaded_files <- character()
  
  aridity_vars <- list(
    "aridity" = "34377245",  # Global Aridity Index
    "pet" = "34377242",      # Potential Evapotranspiration
    "ai_annual" = "34377245",
    "pet_penman" = "34377248",
    "pet_hargreaves" = "34377251"
  )
  
  for (var in variables) {
    if (var %in% names(aridity_vars)) {
      file_id <- aridity_vars[[var]]
      url <- paste0(base_url, file_id)
      dest_file <- file.path(temp_dir, paste0(var, ".zip"))
      
      if (download_file(url, dest_file)) {
        # Unzip
        unzip(dest_file, exdir = temp_dir)
        tif_files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)
        downloaded_files <- c(downloaded_files, tif_files)
      }
    }
  }
  
  return(downloaded_files)
}