# R/var_get_spectre.R
#' Download SPECTRE data (Standardized Precipitation Evapotranspiration Composite Tool for Research)
#' @noRd
var_get_spectre <- function(bbox, resolution, variables, temp_dir, ...) {
  # SPECTRE provides water balance and drought indices
  # This is a hypothetical implementation - adjust URLs as needed
  base_url <- "https://data.spectre.org/"  
  
  downloaded_files <- character()
  
  spectre_vars <- list(
    "spei_01" = "spei_1month.tif",     # SPEI 1-month
    "spei_03" = "spei_3month.tif",     # SPEI 3-month  
    "spei_06" = "spei_6month.tif",     # SPEI 6-month
    "spei_12" = "spei_12month.tif",    # SPEI 12-month
    "spi_01" = "spi_1month.tif",       # SPI 1-month
    "spi_03" = "spi_3month.tif",       # SPI 3-month
    "spi_06" = "spi_6month.tif",       # SPI 6-month
    "spi_12" = "spi_12month.tif",      # SPI 12-month
    "pdsi" = "pdsi_monthly.tif",       # Palmer Drought Severity Index
    "water_balance" = "water_balance_annual.tif"
  )
  
  for (var in variables) {
    if (var %in% names(spectre_vars)) {
      url <- paste0(base_url, spectre_vars[[var]])
      dest_file <- file.path(temp_dir, spectre_vars[[var]])
      
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(downloaded_files)
}