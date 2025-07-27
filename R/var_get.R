# R/var_get.R
#' Download and process environmental variables
#'
#' @param extent Spatial extent (sf object, country name, continent name, or coordinate points)
#' @param source Data source ("worldclim", "chelsa", "worldclim_future", "chelsa_cmip5", "freshwater", etc.)
#' @param resolution Spatial resolution ("30s", "1km", "2.5m", "5m", "10m")
#' @param variables Variables to download ("bioclim", "tmean", "prec", etc.)
#' @param buffer_km Buffer in kilometers around extent
#' @param output_file Optional output file path
#' @param ... Additional arguments passed to specific download functions (e.g., `year` for "esa_landcover")
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
  
  if(!is.null(country) | !is.null(continent)) {
    
    if (is.null(res) || !is.numeric(res) || res < 1 || res != as.integer(res)) {
      stop("Resolution not valid. Select a positive integer > or equal to 1")
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
    
  } else {
  
  if (!((sf::st_geometry_type(shape)=="POINT")[1] & buffer ==0)) {
    
  if (is.null(res) || !is.numeric(res) || res < 1 || res != as.integer(res)) {
    stop("Resolution not valid. Select a positive integer > or equal to 1")
  } else {
    res = res
  } 
    
  }
  
  # 2. Processa extent e griglia target (invariato)
  
  extent_info <- process_extent(
    shape = shape,
    country = country,
    continent = continent,
    buffer = buffer
  )
  
  }
  
  
  if (! extent_info$type=="point") {
  
  target_grid <- create_target_grid(extent_info$bbox, res)

  return(list(target_grid, extent_info$mask, res))
  
  }
  
  if (extent_info$type=="point") {
   
    return(extent_info)
    
  }
  
   
}
