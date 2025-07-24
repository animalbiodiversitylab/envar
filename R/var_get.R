# R/var_get.R
#' Download and process environmental variables
#'
#' @param extent Spatial extent (sf object, country name, continent name, or coordinate points)
#' @param source Data source ("worldclim", "chelsa", "worldclim_future", "chelsa_cmip5", "freshwater", etc.)
#' @param resolution Spatial resolution ("30s", "1km", "2.5m", "5m", "10m")
#' @param variables Variables to download ("bioclim", "tmean", "prec", etc.)
#' @param buffer_km Buffer in kilometers around extent
#' @param output_file Optional output file path
#' @param gcm The General Circulation Model. Used for "worldclim_future" and "chelsa_cmip5". Defaults to NULL.
#' @param ssp The Shared Socioeconomic Pathway (for CMIP6) or RCP scenario (for CMIP5). Used for "worldclim_future" and "chelsa_cmip5". Defaults to NULL.
#' @param time_period The future time period (e.g., "2021-2040"). Used for "worldclim_future" and "chelsa_cmip5". Defaults to NULL.
#' @param ... Additional arguments passed to specific download functions (e.g., `year` for "esa_landcover").
#'
#' @return SpatRaster object or a list of SpatRaster objects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download bioclim variables for Italy
#' bio_italy <- var_get("Italy", source = "worldclim", variables = "bioclim")
#' 
#' # Download future climate data using the new formal arguments
#' future_climate_italy <- var_get(
#'   extent = "Italy",
#'   source = "worldclim_future",
#'   resolution = "30s",
#'   variables = c("tmin", "tmax"),
#'   gcm = "ACCESS-CM2",
#'   ssp = "ssp585",
#'   time_period = "2021-2040"
#' )
#' 
#' # Download freshwater variables for a specific region
#' # Note: The data is downloaded globally but cropped/masked to the extent.
#' freshwater_vars <- var_get(
#'   extent = "Madagascar",
#'   source = "freshwater",
#'   variables = c("elevation", "flow_accumulation", "tmin_monthly_avg")
#' )
#' }
var_get <- function(shape=NULL,
                    country=NULL,
                    continent=NULL,
                    buffer=0,
                    res = NULL,
                    path = NULL
                    ) {
  
  if (is.null(res) || !is.numeric(res) || res < 1 || res != as.integer(res)) {
    stop("Risoluzione non valida. Per favore scegli un intero positivo maggiore di 1.")
  } else {
    res = res
  }  
  
  # 2. Processa extent e griglia target (invariato)
  
  extent_info <- process_extent(
    shape = shape,
    country = country,
    continent = continent,
    buffer = buffer
  )
  
  target_grid <- create_target_grid(extent_info$bbox, res)
  
    # 3. Crea dir temporanea (invariato)
  # temp_dir <- fs::path_temp("envar/grids")
  # fs::dir_create(temp_dir)
  # terra::values(target_grid) <- NA
  
  # terra::writeRaster(target_grid, paste0(temp_dir, "/grid.tif"), overwrite = TRUE)
  # sf::st_write(extent_info$mask, paste0(temp_dir, "/mask.shp"), append = FALSE)
  
  #vecchio da modificare:
  # cli::cli_alert_success("Successfully processed {.val {length(unlist(results))}} layers from {.val {length(results)}} source(s).")
 return(list(target_grid, extent_info$mask, res))
   
}
