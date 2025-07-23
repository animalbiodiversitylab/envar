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
var_get <- function(extent,
                    source = "worldclim",
                    resolution = "1km",
                    variables = NULL,
                    buffer_km = 0,
                    output_file = NULL,
                    gcm = NULL,
                    ssp = NULL,
                    time_period = NULL,
                    ...) {
  
  # 1. Validazione input base (invariato)
  if (is.null(variables)) {
    cli::cli_abort("You must specify `variables`.")
  }
  if (length(source) == 1) {
    if (!is.list(variables)) {
      variables <- setNames(list(variables), source)
    } else if (is.null(names(variables))) {
      cli::cli_abort("If `variables` is a list, it must be named with source names.")
    }
  } else {
    if (!is.list(variables) || is.null(names(variables))) {
      cli::cli_abort("When multiple sources are specified, `variables` must be a named list.")
    }
    if (!all(source %in% names(variables))) {
      cli::cli_abort("Each source in `source` must have a corresponding entry in `variables`.")
    }
  }
  
  # 2. Processa extent e griglia target (invariato)
  extent_info <- process_extent(extent, buffer_km)
  numeric_resolution <- switch(resolution, "1km" = 30, "30s" = 30, "2.5m" = 150, "5m" = 300, "10m" = 600, NA)
  target_grid <- create_target_grid(extent_info$bbox, resolution)
  
  # 3. Crea dir temporanea (invariato)
  temp_dir <- fs::path_temp("envar", format(Sys.time(), "%Y%m%d_%H%M%S"))
  fs::dir_create(temp_dir)
  on.exit(cleanup_temp(temp_dir), add = TRUE)
  
  results <- list()
  
  # 4. Cicla sulle source
  for (src in source) {
    cli::cli_h2("Processing source: {.val {src}}")
    cli::cli_progress_step("Downloading {.val {src}} data...")
    
    extra_args <- list(...)
    
    raw_files <- switch(tolower(src),
                        "worldclim" = var_get_worldclim(
                          bbox = extent_info$bbox, resolution = resolution, 
                          variables = variables[[src]], temp_dir = temp_dir
                        ),
                        "chelsa" = var_get_chelsa(
                          bbox = extent_info$bbox, resolution = resolution,
                          variables = variables[[src]], temp_dir = temp_dir
                        ),
                        "worldclim_future" = {
                          if (is.na(numeric_resolution)) {
                            cli::cli_abort("For 'worldclim_future', resolution must be one of '30s', '2.5m', '5m', '10m'.")
                          }
                          var_get_worldclimfuture(
                            bbox = extent_info$bbox,
                            resolution = numeric_resolution,
                            variables = variables[[src]],
                            temp_dir = temp_dir,
                            gcm = gcm,
                            ssp = ssp,
                            time_period = time_period
                          )
                        },
                        "chelsa_cmip5" = {
                          var_get_chelsa_cmip5(
                            bbox = extent_info$bbox,
                            resolution = resolution,
                            variables = variables[[src]],
                            temp_dir = temp_dir,
                            model = gcm %||% "ACCESS1-3",
                            scenario = ssp %||% "rcp85",
                            period = time_period %||% "2061-2080"
                          )
                        },
                        "freshwater" = var_get_freshwater(
                          variables = variables[[src]],
                          temp_dir = temp_dir
                          # bbox e resolution non sono necessari per il download globale
                        ),
                        "chelsa_bioclimplus" = var_get_chelsa_bioclimplus(
                          bbox = extent_info$bbox, resolution = resolution,
                          variables = variables[[src]], temp_dir = temp_dir
                        ),
                        "topography" = var_get_topography(
                          bbox = extent_info$bbox, resolution = resolution,
                          variables = variables[[src]], temp_dir = temp_dir,
                          source = extra_args$topo_source %||% "gmted2010"
                        ),
                        "esa_landcover" = var_get_esa_landcover(
                          bbox = extent_info$bbox, resolution = resolution,
                          variables = variables[[src]], temp_dir = temp_dir,
                          year = extra_args$year %||% 2020
                        ),
                        "cloud" = var_get_cloud(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        "consensus_landcover" = var_get_consensus_landcover(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        "spectre" = var_get_spectre(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        "heterogeneity" = var_get_heterogeneity(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          indices = list(...)$indices %||% "ndvi",
                          temp_dir = temp_dir
                        ),
                        "hwsd" = var_get_hwsd(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        "aridity" = var_get_aridity(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          temp_dir = temp_dir
                        ),
                        "wind" = var_get_wind(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          height = list(...)$height %||% "50",
                          temp_dir = temp_dir
                        ),
                        "ndvi" = var_get_ndvi(
                          bbox = extent_info$bbox,
                          resolution = resolution,
                          variables = variables[[src]],
                          source = list(...)$ndvi_source %||% "modis",
                          year = list(...)$year %||% 2022,
                          temp_dir = temp_dir
                        ),
                        cli::cli_abort("Unknown source: {.val {src}}")
    )
    
    # Il resto della funzione rimane invariato
    cli::cli_progress_step("Processing environmental layers...")
    processed_stack <- process_layers(
      files = raw_files, target_grid = target_grid, mask = extent_info$mask,
      extent_type = extent_info$type, points = extent_info$points
    )
    
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
  
  cli::cli_alert_success("Successfully processed {.val {length(unlist(results))}} layers from {.val {length(results)}} source(s).")
  
  if (length(results) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}