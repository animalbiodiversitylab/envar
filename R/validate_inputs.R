#' Validate input parameters
#' @noRd
validate_inputs <- function(extent, source, resolution, variables) {
  # Check source
  valid_sources <- c(
    "worldclim", "chelsa", "chelsa_cmip5", "chelsa_bioclimplus",
    "climate_stability", "cloud", "topography", "esa_landcover",
    "consensus_landcover", "spectre", "heterogeneity", "freshwater", 
    "hwsd", "aridity", "wind", "ndvi"
  )
  source_lower <- tolower(source)
  if (!source_lower %in% valid_sources) {
    error_context <- list(
      message = sprintf("Invalid source '%s'. Must be one of: %s", 
                        source, paste(valid_sources, collapse = ", "))
    )
    var_explore(error_context = error_context)
    cli::cli_abort(error_context$message)
  }
  
  # Check resolution
  valid_resolutions <- c("30s", "1km", "2.5m", "5m", "10m")
  if (!resolution %in% valid_resolutions) {
    error_context <- list(
      message = sprintf("Invalid resolution '%s'. Must be one of: %s", 
                        resolution, paste(valid_resolutions, collapse = ", "))
    )
    var_explore(error_context = error_context)
    cli::cli_abort(error_context$message)
  }
  
  # Define source-specific valid variables
  source_vars <- list(
    worldclim = c("bioclim", "tmean", "tmin", "tmax", "prec", "srad", "wind", "vapr"),
    chelsa = c("bioclim", "tas", "tasmin", "tasmax", "pr"),
    chelsa_cmip5 = c("tas", "tasmin", "tasmax", "pr"),
    chelsa_bioclimplus = c("bioclimplus", "gdd0", "gdd5", "gdd10", "nfd", "scd", "lgd"),
    climate_stability = c("temperature_stability", "precipitation_stability"),
    cloud = c("cloud_annual", "cloud_monthly", "cloud_variability", "cloud_interannual"),
    topography = c("elevation", "slope", "aspect", "roughness", "tri", "tpi"),
    esa_landcover = c("landcover", "cropland", "forest", "grassland", "shrubland",
                      "wetland", "urban", "bare", "water", "snow"),
    consensus_landcover = c("evergreen_needleleaf", "evergreen_broadleaf", 
                            "deciduous_needleleaf", "deciduous_broadleaf",
                            "mixed_forest", "shrublands", "herbaceous", 
                            "cultivated", "urban", "barren", "water", "snow_ice"),
    spectre = c("spei_01", "spei_03", "spei_06", "spei_12", "spi_01", "spi_03", 
                "spi_06", "spi_12", "pdsi", "water_balance"),
    heterogeneity = c("cv", "evenness", "range", "shannon", "simpson", "std",
                      "contrast", "correlation", "dissimilarity", "entropy",
                      "homogeneity", "maximum", "uniformity", "variance", "texture_metrics"),
    freshwater = c("flow_accumulation", "flow_direction", "basin",
                   "stream_distance", "elevation"),
    hwsd = c("sand", "silt", "clay", "gravel", "bulk_density", "organic_carbon",
             "ph", "cec", "bs", "sodicity", "salinity", "texture_class"),
    aridity = c("aridity", "pet", "ai_annual", "pet_penman", "pet_hargreaves"),
    wind = c("wind_speed", "power_density", "capacity_factor"),
    ndvi = c("ndvi_monthly", "ndvi_annual", "ndvi_mean", "ndvi_max", "ndvi_min",
             "ndvi_std", "ndvi_amplitude", "greenup_date", "senescence_date")
  )
  
  # Validate variables for the selected source
  valid_vars <- source_vars[[source_lower]]
  if (is.null(valid_vars)) {
    error_context <- list(
      message = sprintf("No variable definitions found for source: %s", source)
    )
    var_explore(source = source, error_context = error_context)
    cli::cli_abort(error_context$message)
  }
  
  if (!all(variables %in% valid_vars)) {
    invalid <- variables[!variables %in% valid_vars]
    error_context <- list(
      message = sprintf("Invalid variables for '%s': %s", 
                        source, paste(invalid, collapse = ", "))
    )
    var_explore(source = source, what = "variables", error_context = error_context)
    cli::cli_abort(error_context$message)
  }
}
