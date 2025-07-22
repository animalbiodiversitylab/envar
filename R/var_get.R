# R/var_get.R
#' Download and process environmental variables
#'
#' @param extent Spatial extent (sf object, country name, continent name, or coordinate points)
#' @param source Data source ("worldclim", "chelsa")
#' @param resolution Spatial resolution ("30s", "1km", "2.5m", "5m", "10m")
#' @param variables Variables to download ("bioclim", "tmean", "prec", etc.)
#' @param buffer_km Buffer in kilometers around extent
#' @param output_file Optional output file path
#' @param ... Additional arguments passed to specific download functions
#'
#' @return SpatRaster object
#' @export
#'
#' @examples
#' \dontrun{
#' # Download bioclim variables for Italy
#' bio_italy <- var_get("Italy", source = "worldclim", variables = "bioclim")
#' 
#' # Download with shapefile
#' shp <- sf::st_read("myarea.shp")
#' bio_area <- var_get(shp, source = "chelsa", variables = "bioclim")
#' }
var_get <- function(extent,
                    source = "worldclim",
                    resolution = "1km",
                    variables = "bioclim",
                    buffer_km = 0,
                    output_file = NULL,
                    ...) {
  
  # 1. Validate inputs
  validate_inputs(extent, source, resolution, variables)
  
  # 2. Process extent and create target grid
  extent_info <- process_extent(extent, buffer_km)
  target_grid <- create_target_grid(extent_info$bbox, resolution)

  # 3. Create temp directory for downloads
  temp_dir <- fs::path_temp("envar", format(Sys.time(), "%Y%m%d_%H%M%S"))
  fs::dir_create(temp_dir)
  on.exit(cleanup_temp(temp_dir), add = TRUE)
  
  # 4. Download data based on source
  cli::cli_progress_step("Downloading {.val {source}} data...")
  
  raw_files <- switch(tolower(source),
                      "worldclim" = var_get_worldclim(extent_info$bbox, resolution, variables, temp_dir),
                      "chelsa" = var_get_chelsa(extent_info$bbox, resolution, variables, temp_dir),
                      cli::cli_abort("Unknown source: {.val {source}}")
  )
  
  # 5. Process downloaded files
  cli::cli_progress_step("Processing environmental layers...")
  
  processed_stack <- process_layers(
    raw_files, 
    target_grid, 
    extent_info$mask,
    extent_info$type,
    extent_info$points
  )
  
  # 6. Save if requested
  if (!is.null(output_file)) {
    cli::cli_progress_step("Saving to {.path {output_file}}...")
    terra::writeRaster(processed_stack, output_file, overwrite = TRUE)
  }
  
  cli::cli_success("Successfully processed {.val {terra::nlyr(processed_stack)}} layers")
  
  return(processed_stack)
}
