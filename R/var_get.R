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
                    variables = NULL,
                    buffer_km = 0,
                    output_file = NULL,
                    ...) {
  
  # 1. Validazione input base
  if (is.null(variables)) {
    cli::cli_abort("You must specify `variables`.")
  }
  
  # Se una sola source: trasformo variables in lista named
  if (length(source) == 1) {
    if (!is.list(variables)) {
      variables <- setNames(list(variables), source)
    } else if (is.null(names(variables))) {
      cli::cli_abort("If `variables` is a list, it must be named with source names.")
    }
  } else {
    # Più source: variables deve essere lista named
    if (!is.list(variables) || is.null(names(variables))) {
      cli::cli_abort("When multiple sources are specified, `variables` must be a named list.")
    }
    if (!all(source %in% names(variables))) {
      cli::cli_abort("Each source in `source` must have a corresponding entry in `variables`.")
    }
  }
  
  # 2. Processa extent e griglia target
  extent_info <- process_extent(extent, buffer_km)
  target_grid <- create_target_grid(extent_info$bbox, resolution)
  
  # 3. Crea dir temporanea
  temp_dir <- fs::path_temp("envar", format(Sys.time(), "%Y%m%d_%H%M%S"))
  fs::dir_create(temp_dir)
  on.exit(cleanup_temp(temp_dir), add = TRUE)
  
  results <- list()
  
  # 4. Cicla sulle source
  for (src in source) {
    cli::cli_h2("Processing source: {.val {src}}")
    cli::cli_progress_step("Downloading {.val {src}} data...")
    
    raw_files <- switch(tolower(src),
                        "worldclim" = var_get_worldclim(
                          bbox = extent_info$bbox,
                          resolution = resolution, 
                          variables = variables[[src]], 
                          temp_dir = temp_dir
                        ),
                        "chelsa" = var_get_chelsa(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        cli::cli_abort("Unknown source: {.val {src}}")
    )
    
    cli::cli_progress_step("Processing environmental layers...")
    
    processed_stack <- process_layers(
      files = raw_files,
      target_grid = target_grid,
      mask = extent_info$mask,
      extent_type = extent_info$type,
      points = extent_info$points
    )
    
    # 5. Salva se richiesto
    if (!is.null(output_file)) {
      if (length(source) == 1 && is.character(output_file)) {
        out_path <- output_file
      } else if (is.list(output_file) && !is.null(output_file[[src]])) {
        out_path <- output_file[[src]]
      } else {
        out_path <- fs::path_ext_set(fs::path(temp_dir, paste0("output_", src)), ".tif")
        cli::cli_alert_info("No output file provided for {.val {src}}. Saving temporarily to {.path {out_path}}")
      }
      cli::cli_progress_step("Saving to {.path {out_path}}...")
      terra::writeRaster(processed_stack, out_path, overwrite = TRUE)
    }
    
    results[[src]] <- processed_stack
  }
  
  cli::cli_success("Successfully processed {.val {length(unlist(results))}} layers from {.val {length(results)}} source(s).")
  
  if (length(results) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}
