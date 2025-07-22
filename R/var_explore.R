# R/var_explore.R
#' Explore available variables and parameters
#'
#' @param source Data source to explore
#' @param what What to explore ("sources", "variables", "resolutions")
#' @param error_context Error context from var_get (if called from error handler)
#'
#' @return Prints helpful information
#' @export
#'
#' @examples
#' # Show available sources
#' var_explore()
#' 
#' # Show variables for WorldClim
#' var_explore(source = "worldclim", what = "variables")
var_explore <- function(source = NULL, what = "sources", error_context = NULL) {
  
  if (!is.null(error_context)) {
    cli::cli_h1("Error in var_get()")
    cli::cli_alert_danger(error_context$message)
    cli::cli_text("")
    
    # Provide specific help based on error type
    if (grepl("source", error_context$message, ignore.case = TRUE)) {
      what <- "sources"
    } else if (grepl("resolution", error_context$message, ignore.case = TRUE)) {
      what <- "resolutions"
    } else if (grepl("variable", error_context$message, ignore.case = TRUE)) {
      what <- "variables"
    }
  }
  
  switch(what,
         "sources" = {
           cli::cli_h2("Available data sources:")
           cli::cli_ul(c(
             "{.strong worldclim}: WorldClim 2.1 climate data",
             "{.strong chelsa}: CHELSA climatologies v2.1"
           ))
         },
         
         "variables" = {
           if (is.null(source)) {
             cli::cli_alert_info("Specify a source to see available variables")
             return(invisible())
           }
           
           cli::cli_h2("Available variables for {.val {source}}:")
           
           if (tolower(source) == "worldclim") {
             cli::cli_ul(c(
               "{.strong bioclim}: 19 bioclimatic variables (bio1-bio19)",
               "{.strong tmean}: Monthly mean temperature",
               "{.strong tmin}: Monthly minimum temperature",
               "{.strong tmax}: Monthly maximum temperature",
               "{.strong prec}: Monthly precipitation",
               "{.strong srad}: Solar radiation",
               "{.strong wind}: Wind speed",
               "{.strong vapr}: Water vapor pressure"
             ))
           } else if (tolower(source) == "chelsa") {
             cli::cli_ul(c(
               "{.strong bioclim}: 19 bioclimatic variables (bio1-bio19)",
               "{.strong tas}: Mean temperature",
               "{.strong tasmin}: Minimum temperature",
               "{.strong tasmax}: Maximum temperature",
               "{.strong pr}: Precipitation"
             ))
           }
         },
         
         "resolutions" = {
           cli::cli_h2("Available resolutions:")
           cli::cli_ul(c(
             "{.strong 30s}: ~1 km at equator (30 arc-seconds)",
             "{.strong 1km}: Resampled to exactly 1 km",
             "{.strong 2.5m}: 2.5 arc-minutes (~5 km)",
             "{.strong 5m}: 5 arc-minutes (~10 km)",
             "{.strong 10m}: 10 arc-minutes (~20 km)"
           ))
           cli::cli_alert_info("Default target resolution is always 30 arc-seconds")
         }
  )
  
  if (!is.null(error_context)) {
    cli::cli_text("")
    cli::cli_h2("Example usage:")
    cli::cli_code('bio_italy <- var_get("Italy", source = "worldclim", variables = "bioclim")')
  }
}