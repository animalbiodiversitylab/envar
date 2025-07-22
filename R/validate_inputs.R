# R/validate_inputs.R
#' Validate input parameters
#' @noRd
validate_inputs <- function(extent, source, resolution, variables) {
  # Check source
  valid_sources <- c("worldclim", "chelsa")
  if (!tolower(source) %in% valid_sources) {
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
  
  # Check variables based on source
  if (tolower(source) == "worldclim") {
    valid_vars <- c("bioclim", "tmean", "tmin", "tmax", "prec", "srad", "wind", "vapr")
  } else {
    valid_vars <- c("bioclim", "tas", "tasmin", "tasmax", "pr")
  }
  
  if (!all(variables %in% valid_vars)) {
    invalid <- variables[!variables %in% valid_vars]
    error_context <- list(
      message = sprintf("Invalid variables for %s: %s", 
                        source, paste(invalid, collapse = ", "))
    )
    var_explore(source = source, what = "variables", error_context = error_context)
    cli::cli_abort(error_context$message)
  }
}