# R/var_explore.R
#' Explore available variables and parameters
#'
#' @param source Data source to explore
#' @param what What to explore ("sources", "variables", "resolutions")
#' @param error_context Error context from par_set (if called from error handler)
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
    cli::cli_h1("Error in par_set()")
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
           cli::cli_text("")
           
           cli::cli_h3("Climate Data:")
           cli::cli_ul(c(
             "{.strong worldclim}: WorldClim 2.1 climate data",
             "{.strong chelsa}: CHELSA climatologies v2.1", 
             "{.strong chelsa_cmip5}: CHELSA CMIP5 future projections",
             "{.strong chelsa_bioclimplus}: Extended bioclimatic variables",
             "{.strong climate_stability}: Climate stability indices"
           ))
           
           cli::cli_h3("Land Cover:")
           cli::cli_ul(c(
             "{.strong esa_landcover}: ESA Land Cover CCI",
             "{.strong consensus_landcover}: Consensus land cover (Tuanmu & Jetz 2014)"
           ))
           
           cli::cli_h3("Terrain & Cloud:")
           cli::cli_ul(c(
             "{.strong topography}: Elevation, slope, aspect, roughness, TRI, TPI",
             "{.strong cloud}: Cloud cover frequency from MODIS"
           ))
           
           cli::cli_h3("Vegetation:")
           cli::cli_ul(c(
             "{.strong ndvi}: NDVI time series from MODIS/GIMMS",
             "{.strong heterogeneity}: Spectral heterogeneity metrics"
           ))
           
           cli::cli_h3("Water & Drought:")
           cli::cli_ul(c(
             "{.strong spectre}: SPEI, SPI, PDSI drought indices",
             "{.strong freshwater}: HydroSHEDS hydrological variables",
             "{.strong aridity}: Global Aridity Index and PET"
           ))
           
           cli::cli_h3("Other Environmental:")
           cli::cli_ul(c(
             "{.strong hwsd}: Harmonized World Soil Database",
             "{.strong wind}: Global Wind Atlas"
           ))
         },
         
         "variables" = {
           if (is.null(source)) {
             cli::cli_alert_info("Specify a source to see available variables")
             return(invisible())
           }
           
           cli::cli_h2("Available variables for {.val {source}}:")
           
           switch(tolower(source),
                  "worldclim" = {
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
                  },
                  
                  "chelsa" = {
                    cli::cli_ul(c(
                      "{.strong bioclim}: 19 bioclimatic variables (bio1-bio19)",
                      "{.strong tas}: Mean temperature",
                      "{.strong tasmin}: Minimum temperature",
                      "{.strong tasmax}: Maximum temperature",
                      "{.strong pr}: Precipitation"
                    ))
                  },
                  
                  "chelsa_cmip5" = {
                    cli::cli_ul(c(
                      "{.strong tas}: Mean temperature (monthly)",
                      "{.strong tasmin}: Minimum temperature (monthly)",
                      "{.strong tasmax}: Maximum temperature (monthly)",
                      "{.strong pr}: Precipitation (monthly)"
                    ))
                    cli::cli_alert_info("Additional parameters: model, scenario, period")
                  },
                  
                  "chelsa_bioclimplus" = {
                    cli::cli_ul(c(
                      "{.strong bioclimplus}: Extended bioclimatic variables (bio20-bio42)",
                      "{.strong gdd0}: Growing degree days (base 0°C)",
                      "{.strong gdd5}: Growing degree days (base 5°C)",
                      "{.strong gdd10}: Growing degree days (base 10°C)",
                      "{.strong nfd}: Number of frost days",
                      "{.strong scd}: Snow cover days",
                      "{.strong lgd}: Length of growing season"
                    ))
                  },
                  
                  "climate_stability" = {
                    cli::cli_ul(c(
                      "{.strong temperature_stability}: Temperature stability index",
                      "{.strong precipitation_stability}: Precipitation stability index"
                    ))
                  },
                  
                  "cloud" = {
                    cli::cli_ul(c(
                      "{.strong cloud_annual}: Mean annual cloud frequency",
                      "{.strong cloud_monthly}: Monthly cloud frequency (12 layers)",
                      "{.strong cloud_variability}: Intra-annual cloud variability",
                      "{.strong cloud_interannual}: Inter-annual cloud variability"
                    ))
                  },
                  
                  "topography" = {
                    cli::cli_ul(c(
                      "{.strong elevation}: Digital elevation model",
                      "{.strong slope}: Terrain slope",
                      "{.strong aspect}: Slope aspect",
                      "{.strong roughness}: Surface roughness",
                      "{.strong tri}: Terrain Ruggedness Index",
                      "{.strong tpi}: Topographic Position Index"
                    ))
                    cli::cli_alert_info("Additional parameter: topo_source ('gmted2010' or 'srtm')")
                  },
                  
                  "esa_landcover" = {
                    cli::cli_ul(c(
                      "{.strong landcover}: Land cover classification map",
                      "{.strong cropland}: Cropland fraction",
                      "{.strong forest}: Forest fraction",
                      "{.strong grassland}: Grassland fraction",
                      "{.strong shrubland}: Shrubland fraction",
                      "{.strong wetland}: Wetland fraction",
                      "{.strong urban}: Urban fraction",
                      "{.strong bare}: Bare soil fraction",
                      "{.strong water}: Water fraction",
                      "{.strong snow}: Snow/ice fraction"
                    ))
                    cli::cli_alert_info("Additional parameter: year (1992-2020)")
                  },
                  
                  "consensus_landcover" = {
                    cli::cli_ul(c(
                      "{.strong evergreen_needleleaf}: Evergreen needleleaf forest",
                      "{.strong evergreen_broadleaf}: Evergreen broadleaf forest",
                      "{.strong deciduous_needleleaf}: Deciduous needleleaf forest",
                      "{.strong deciduous_broadleaf}: Deciduous broadleaf forest",
                      "{.strong mixed_forest}: Mixed forest",
                      "{.strong shrublands}: Shrublands",
                      "{.strong herbaceous}: Herbaceous vegetation",
                      "{.strong cultivated}: Cultivated and managed vegetation",
                      "{.strong urban}: Urban/built-up",
                      "{.strong barren}: Barren",
                      "{.strong water}: Open water",
                      "{.strong snow_ice}: Snow and ice"
                    ))
                  },
                  
                  "spectre" = {
                    cli::cli_ul(c(
                      "{.strong spei_01}: SPEI 1-month",
                      "{.strong spei_03}: SPEI 3-month",
                      "{.strong spei_06}: SPEI 6-month",
                      "{.strong spei_12}: SPEI 12-month",
                      "{.strong spi_01}: SPI 1-month",
                      "{.strong spi_03}: SPI 3-month",
                      "{.strong spi_06}: SPI 6-month",
                      "{.strong spi_12}: SPI 12-month",
                      "{.strong pdsi}: Palmer Drought Severity Index",
                      "{.strong water_balance}: Annual water balance"
                    ))
                  },
                  
                  "heterogeneity" = {
                    cli::cli_ul(c(
                      "{.strong cv}: Coefficient of variation",
                      "{.strong evenness}: Pielou's evenness",
                      "{.strong range}: Value range",
                      "{.strong shannon}: Shannon diversity index",
                      "{.strong simpson}: Simpson diversity index",
                      "{.strong std}: Standard deviation",
                      "{.strong contrast}: GLCM contrast",
                      "{.strong correlation}: GLCM correlation",
                      "{.strong dissimilarity}: GLCM dissimilarity",
                      "{.strong entropy}: GLCM entropy",
                      "{.strong homogeneity}: GLCM homogeneity",
                      "{.strong maximum}: Maximum value",
                      "{.strong uniformity}: Uniformity",
                      "{.strong variance}: Variance",
                      "{.strong texture_metrics}: All GLCM texture metrics"
                    ))
                    cli::cli_alert_info("Additional parameter: indices (e.g., c('ndvi', 'evi'))")
                  },
                  
                  "freshwater" = {
                    cli::cli_ul(c(
                      "{.strong flow_accumulation}: Flow accumulation",
                      "{.strong flow_direction}: Flow direction",
                      "{.strong basin}: River basin delineation",
                      "{.strong stream_distance}: Distance to nearest stream",
                      "{.strong elevation}: Hydrologically conditioned elevation"
                    ))
                  },
                  
                  "hwsd" = {
                    cli::cli_ul(c(
                      "{.strong sand}: Sand content (%)",
                      "{.strong silt}: Silt content (%)",
                      "{.strong clay}: Clay content (%)",
                      "{.strong gravel}: Gravel content (%)",
                      "{.strong bulk_density}: Bulk density (kg/m³)",
                      "{.strong organic_carbon}: Organic carbon (% weight)",
                      "{.strong ph}: Soil pH",
                      "{.strong cec}: Cation exchange capacity (cmol/kg)",
                      "{.strong bs}: Base saturation (%)",
                      "{.strong sodicity}: Sodicity (ESP)",
                      "{.strong salinity}: Salinity (dS/m)",
                      "{.strong texture_class}: USDA texture class"
                    ))
                  },
                  
                  "aridity" = {
                    cli::cli_ul(c(
                      "{.strong aridity}: Global Aridity Index",
                      "{.strong pet}: Potential Evapotranspiration",
                      "{.strong ai_annual}: Annual aridity index",
                      "{.strong pet_penman}: PET Penman-Monteith",
                      "{.strong pet_hargreaves}: PET Hargreaves"
                    ))
                  },
                  
                  "wind" = {
                    cli::cli_ul(c(
                      "{.strong wind_speed}: Mean wind speed (m/s)",
                      "{.strong power_density}: Wind power density (W/m²)",
                      "{.strong capacity_factor}: Capacity factor"
                    ))
                    cli::cli_alert_info("Additional parameter: height (e.g., '50', '100', '150')")
                  },
                  
                  "ndvi" = {
                    cli::cli_ul(c(
                      "{.strong ndvi_monthly}: Monthly NDVI (12 layers)",
                      "{.strong ndvi_annual}: Annual NDVI composite",
                      "{.strong ndvi_mean}: Mean NDVI",
                      "{.strong ndvi_max}: Maximum NDVI",
                      "{.strong ndvi_min}: Minimum NDVI",
                      "{.strong ndvi_std}: NDVI standard deviation",
                      "{.strong ndvi_amplitude}: NDVI amplitude",
                      "{.strong greenup_date}: Green-up date (DOY)",
                      "{.strong senescence_date}: Senescence date (DOY)"
                    ))
                    cli::cli_alert_info("Additional parameters: ndvi_source ('modis' or 'gimms'), year")
                  },
                  
                  {
                    cli::cli_alert_warning("Unknown source: {.val {source}}")
                  }
           )
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
    cli::cli_code('bio_italy <- par_set("Italy", source = "worldclim", variables = "bioclim")')
  }
}
