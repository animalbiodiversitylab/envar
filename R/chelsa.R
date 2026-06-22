# R/chelsa.R

#' Download CHELSA climate data
#'
#' This function downloads, processes, and extracts variables from the
#' CHELSA (Climatologies at High Resolution for the Earth's Land Surface Areas) dataset.
#'
#' @details
#' \strong{Available variables}
#' 
#' Please note the distinction between "Monthly" time-series data and "Climatologies". 
#' Unlike other functions in this package, there is only one code-name for each variable 
#' and no working synonyms. The meaning of each variable code-name is provided in parentheses.
#'
#' \strong{Monthly Time-Series (Available from 1979 onwards)}
#' \itemize{
#'   \item 1 - "pr" (Precipitation amount; mass per unit area)
#'   \item 2 - "tas" (Mean daily air temperature at 2 meters)
#'   \item 3 - "tasmax" (Mean daily maximum air temperature at 2 meters)
#'   \item 4 - "tasmin" (Mean daily minimum air temperature at 2 meters)
#'   \item 5 - "hurs" (Near-surface relative humidity)
#'   \item 6 - "clt" (Total cloud cover at surface; considers entire atmospheric column)
#'   \item 7 - "sfcWind" (Near-surface wind speed at 10m above ground)
#'   \item 8 - "vpd" (Vapor pressure deficit)
#'   \item 9 - "rsds" (Surface downwelling shortwave flux in air)
#'   \item 10 - "pet_penman" (Potential evapotranspiration; Penman-Monteith equation)
#'   \item 11 - "cmi" (Climate Moisture Index)
#'   \item 12 - "swb" (Site water balance; cumulative available water)
#' }
#'
#' \strong{Climatologies & Derived Indices (1981-2010, 2011-2040, 2041-2070, 2071-2100)}
#' 
#' \strong{Cloud Cover}
#' \itemize{
#'   \item 13 - "clt_mean" (Mean monthly total cloud cover over 1 year)
#'   \item 14 - "clt_max" (Maximum monthly total cloud cover)
#'   \item 15 - "clt_min" (Minimum monthly total cloud cover)
#'   \item 16 - "clt_range" (Annual range of monthly total cloud cover)
#' }
#' 
#' \strong{Climate Moisture Index}
#' \itemize{
#'   \item 17 - "cmi_mean" (Mean monthly climate moisture index)
#'   \item 18 - "cmi_max" (Maximum monthly climate moisture index; highest surplus)
#'   \item 19 - "cmi_min" (Minimum monthly climate moisture index; highest deficit)
#'   \item 20 - "cmi_range" (Annual range of monthly climate moisture index)
#' }
#' 
#' \strong{Relative Humidity}
#' \itemize{
#'   \item 21 - "hurs_mean" (Mean monthly near-surface relative humidity)
#'   \item 22 - "hurs_max" (Maximum monthly near-surface relative humidity)
#'   \item 23 - "hurs_min" (Minimum monthly near-surface relative humidity)
#'   \item 24 - "hurs_range" (Annual range of monthly near-surface relative humidity)
#' }
#' 
#' \strong{Potential Evapotranspiration}
#' \itemize{
#'   \item 25 - "pet_penman_mean" (Mean monthly PET)
#'   \item 26 - "pet_penman_max" (Maximum monthly PET)
#'   \item 27 - "pet_penman_min" (Minimum monthly PET)
#'   \item 28 - "pet_penman_range" (Annual range of monthly PET)
#' }
#' 
#' \strong{Solar Radiation}
#' \itemize{
#'   \item 29 - "rsds_mean" (Mean monthly surface downwelling shortwave flux)
#'   \item 30 - "rsds_max" (Maximum monthly surface downwelling shortwave flux)
#'   \item 31 - "rsds_min" (Minimum monthly surface downwelling shortwave flux)
#'   \item 32 - "rsds_range" (Annual range of monthly surface downwelling shortwave flux)
#' }
#' 
#' \strong{Wind Speed}
#' \itemize{
#'   \item 33 - "sfcWind_mean" (Mean monthly near-surface wind speed)
#'   \item 34 - "sfcWind_max" (Maximum monthly near-surface wind speed)
#'   \item 35 - "sfcWind_min" (Minimum monthly near-surface wind speed)
#'   \item 36 - "sfcWind_range" (Annual range of monthly near-surface wind speed)
#' }
#' 
#' \strong{Vapor Pressure Deficit}
#' \itemize{
#'   \item 37 - "vpd_mean" (Mean monthly vapor pressure deficit)
#'   \item 38 - "vpd_max" (Maximum monthly vapor pressure deficit)
#'   \item 39 - "vpd_min" (Minimum monthly vapor pressure deficit)
#'   \item 40 - "vpd_range" (Annual range of monthly vapor pressure deficit)
#' }
#' 
#' \strong{Growing Season Characteristics (TREELIM model)}
#' \itemize{
#'   \item 41 - "gsl" (Growing season length; days)
#'   \item 42 - "gsp" (Accumulated precipitation during growing season)
#'   \item 43 - "gst" (Mean temperature of the growing season)
#'   \item 44 - "fgd" (First day of the growing season; Julian day)
#'   \item 45 - "lgd" (Last day of the growing season; Julian day)
#' }
#' 
#' \strong{Growing Degree Days (GDD)}
#' \itemize{
#'   \item 46 - "gdd0" (Heat sum of all days > 0°C accumulated over 1 year)
#'   \item 47 - "gdd5" (Heat sum of all days > 5°C accumulated over 1 year)
#'   \item 48 - "gdd10" (Heat sum of all days > 10°C accumulated over 1 year)
#'   \item 49 - "ngd0" (Number of days with tas > 0°C)
#'   \item 50 - "ngd5" (Number of days with tas > 5°C)
#'   \item 51 - "ngd10" (Number of days with tas > 10°C)
#'   \item 52 - "gdgfgd0" (First growing degree day > 0°C; Julian day)
#'   \item 53 - "gdgfgd5" (First growing degree day > 5°C; Julian day)
#'   \item 54 - "gdgfgd10" (First growing degree day > 10°C; Julian day)
#'   \item 55 - "gddlgd0" (Last growing degree day > 0°C; Julian day)
#'   \item 56 - "gddlgd5" (Last growing degree day > 5°C; Julian day)
#'   \item 57 - "gddlgd10" (Last growing degree day > 10°C; Julian day)
#' }
#' 
#' \strong{Snow and Frost}
#' \itemize{
#'   \item 58 - "scd" (Snow cover days; count)
#'   \item 59 - "swe" (Snow water equivalent; accumulated amount of liquid water if snow melted)
#'   \item 60 - "fcf" (Frost change frequency; events where tmin/tmax cross 0°C)
#' }
#' 
#' \strong{Biological Productivity}
#' \itemize{
#'   \item 61 - "npp" (Net primary productivity; g C m^-2 yr^-1)
#' }
#' 
#' \strong{Climate Classifications}
#' \itemize{
#'   \item 62 - "kg0" (Köppen-Geiger climate category)
#'   \item 63 - "kg1" (Köppen-Geiger without As/Aw differentiation)
#'   \item 64 - "kg2" (Köppen-Geiger after Peel et al. 2007)
#'   \item 65 - "kg3" (Wissmann 1939 classification)
#'   \item 66 - "kg4" (Thornthwaite 1931 classification)
#'   \item 67 - "kg5" (Troll-Pfaffen classification)
#' }
#'
#' \strong{Citation:}\cr
#' Standard bioclimatic variables: Karger D, Conrad O, Böhner J et al (2017). "Climatologies at high resolution for the earth’s land surface areas." Scientific Data 4, 170122. https://doi.org/10.1038/sdata.2017.122 \cr
#' 
#' BIOCLIM+ dataset: Brun P, Zimmermann NE, Hari C, Pellissier L, Karger DN (2022). "Global climate-related predictors at kilometer resolution for the past and future." Earth System Science Data 14, 5573-5603. https://doi.org/10.5194/essd-14-5573-2022
#'
#' Note: Users should verify the terms of use for CHELSA data provided
#' at https://chelsa-climate.org/
#'
#' @param x The output from `par_set()` defining the area or locations for extraction,
#' the reference system, and the buffer.
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param years A character or numeric vector of years or year ranges (e.g., "1981-2010", 2015).
#' @param months A numeric vector (1–12) specifying which months to download.
#'        If NULL and `years` are single years, all 12 months are downloaded.
#' @param gcm General Circulation Model(s) for future projections.
#' @param rcp Representative Concentration Pathway, given as the radiative-forcing
#'        level (e.g., \code{2.6}, \code{4.5}, \code{6.0}, \code{8.5}). For CMIP5
#'        projections (year ranges \code{"2041-2060"}, \code{"2061-2080"}) it selects
#'        the RCP directly. For CMIP6/BIOCLIM+ projections it is combined with
#'        \code{ssp} to build the scenario code (e.g., \code{ssp = 5} and
#'        \code{rcp = 8.5} request the \code{ssp585} scenario).
#' @param ssp Shared Socioeconomic Pathway family for CMIP6/BIOCLIM+ data
#'        (e.g., \code{1}, \code{2}, \code{3}, \code{5}). Combined with \code{rcp}
#'        as described above. A complete code such as \code{"585"} may also be
#'        supplied directly (with \code{rcp = NULL}).
#' @param cruts_years Numeric vector. Years to download from CHELSAcruts (must be 1901–2016).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- par_set(country= "Italy", crs=3035) %>%
#' chelsa(vars=c("pr", "tas"), years = 2018, months = 1)
#'    }
#' @export

chelsa <- function(x, vars, years = NULL, months = NULL, gcm = NULL, rcp = NULL, 
                   ssp = NULL, cruts_years = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using CHELSA data.\n",
    "Citation if using standard climatology: Karger D, Conrad O, Böhner J et al (2017). Climatologies at high resolution for the earth’s land surface areas. Scientific Data 4, 170122.\n",
    "DOI: {.url https://doi.org/10.1038/sdata.2017.122}\n",
    "Citation if using the BIOCLIM+ dataset: Brun P, Zimmermann NE, Hari C, Pellissier L, Karger DN (2022). Global climate-related predictors at kilometer resolution for the past and future. Earth System Science Data 14, 5573-5603.\n",
    "DOI: {.url https://doi.org/10.5194/essd-14-5573-2022}" 
  ))
  
  par_list <- get_par(x)
  
  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res <- par_list$res
    crs <- par_list$crs
    is_global <- isTRUE(par_list$is_global)
    is_raster_input <- TRUE
    set_na = par_list$set_na
    path = par_list$path
    land = par_list$land
    # Track cumulative global extent
    current_global_extent <- par_list$global_extent
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs <- par_list$crs
    is_global <- FALSE
    is_raster_input <- FALSE
    current_global_extent <- NULL
    path = par_list$path
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- envar_grids_dir()
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {user_name}}...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
          #   fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {user_name}}...")
      
      # Process layer based on whether we're doing global or regional processing
      result <- process_raster_layer(
        layer = layer,
        grid = grid,
        mask = mask,
        res = res,
        crs = crs,
        is_global = is_global,
        current_extent = current_global_extent
      )
      
      if (is_global) {
        # For global processing, result is a list with layer and extent
        layer1 <- result$layer
        new_extent <- result$extent
        
        # Update the cumulative global extent
        current_global_extent <<- new_extent
        
        # If we have existing layers and extent changed, crop them
        if (!is.null(processed_stack)) {
          processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        # For regional processing, result is just the layer
        layer1 <- result
      }
      
      # Assign user-requested name to layer
      names(layer1) <- user_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) {
        #   fs::file_delete(dest_file)
      }
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
          #   fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
        names(extracted)[ncol(extracted)] <- user_name
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      
      rm(extracted)
      gc()
      if (!is_global) {
        #    fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of CHELSA data...")
  
  # Handle BIO expansion if requested
  if ("bio" %in% vars) {
    bio_range <- readline(prompt = "Enter BIO numbers (e.g., 1:19 or 3:5): ")
    bio_nums <- eval(parse(text = bio_range))
    vars <- setdiff(vars, "bio")
    vars <- c(vars, paste0("bio", bio_nums))
  }
  
  # Handle CHELSAcruts (1901–2016)
  if (!is.null(cruts_years)) {
    if (any(cruts_years < 1901) || any(cruts_years > 2016)) {
      cli::cli_abort("CHELSAcruts data is only available for years 1901–2016.")
    }
    
    cli::cli_alert_info("Downloading CHELSAcruts data...")
    base_url_cruts <- "https://os.zhdk.cloud.switch.ch/chelsav1/chelsa_cruts"
    months_to_download <- if (is.null(months)) 1:12 else months
    
    for (var in vars) {
      # Map var to CHELSAcruts naming
      var_cruts <- switch(var,
                          "pr" = "prec",
                          "tasmax" = "tmax",
                          "tasmin" = "tmin",
                          var)
      
      for (year in cruts_years) {
        for (month in months_to_download) {
          filename <- sprintf("CHELSAcruts_%s_%d_%d_V.1.0.tif", var_cruts, month, year)
          url <- sprintf("%s/%s/%s", base_url_cruts, var_cruts, filename)
          canon <- var
          user_name <- paste0(var, "_", year, "_", month)
          dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
          handle_file(url, dest_file, canon, user_name)
        }
      }
    }
  }
  
  # Standard CHELSA (v1/v2) Processing
  if (!is.null(years) && length(years) > 0) {
    
    for (var in vars) {
      for (y in years) {
        year_str <- as.character(y)
        is_range <- grepl("-", year_str)
        
        # Historical CMIP5 (chelsav1)
        if (year_str %in% c("2041-2060", "2061-2080")) {
          if (is.null(gcm) || is.null(rcp)) {
            missing <- c("gcm", "rcp")[c(is.null(gcm), is.null(rcp))]
            cli::cli_abort(c(
              "CHELSA CMIP5 projections (years {.val {year_str}}) require both {.arg gcm} and {.arg rcp}.",
              "x" = "Missing: {.arg {missing}}.",
              "i" = "e.g. {.code chelsa(vars = \"bio1\", years = \"2041-2060\", gcm = \"ACCESS1-3\", rcp = 8.5)}."
            ))
          }
          for (g in gcm) {
            for (r in rcp) {
              # Accept rcp as a forcing level (e.g. 8.5) or scaled code (e.g. 85)
              r_code <- rcp_code(r)
              base_url <- "https://os.zhdk.cloud.switch.ch/chelsav1/cmip5"
              
              # Map var to folder name
              if (var == "pr") {
                var1 <- "prec"
              } else if (var == "tas") {
                var1 <- "temp"
              } else if (var == "tasmax") {
                var1 <- "tmax"
              } else if (var == "tasmin") {
                var1 <- "tmin"
              } else if (grepl("^bio\\d+$", var)) {
                var1 <- "bio"
              } else {
                var1 <- var
              }
              
              # Decide whether to add _V1.2 in the filename
              version_suffix <- if (var == "pr") "" else "_V1.2"
              
              filename <- sprintf("CHELSA_%s_mon_%s_rcp%s_r1i1p1_g025.nc_%s%s.tif",
                                  var, g, r_code, year_str, version_suffix)

              url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
              canon <- var
              user_name <- paste0(var, "_", year_str, "_", g, "_rcp", r_code)
              dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
              handle_file(url, dest_file, canon, user_name)
            }
          }
        }
        
        # CHELSAv2 Climatologies
        if (year_str %in% c("1981-2010", "2011-2040", "2041-2070", "2071-2100")) {
          
          if (year_str == "1981-2010") {
            base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies"
            
            # Map var to folder name
            var1 <- var
            
            if (grepl("^bio\\d+$", var) | 
                var %in% c("ai", "clt_max", "clt_min",
                           "clt_mean", "clt_range", "cmi_max", "cmi_min",
                           "cmi_mean", "cmi_range", "fcf", "fgd", "gdd0", "gdd5",
                           "gdd10", "gddlgd0", "gddlgd5", "gddlgd10",
                           "gddfgd0", "gddfgd5", "gddfgd10", "gls", "gsp", "gst",
                           "hurs_max", "hurs_min", "hurs_mean", "hurs_range",
                           "kg0", "kg1", "kg2", "kg3", "kg4", "kg5",
                           "lgd", "ngd0", "ngd5", "ngd10", "npp",
                           "pet_penman_max", "pet_penman_min", "pet_penman_mean",
                           "pet_penman_range", "rsds_max", "rsds_min", "rsds_mean", "rsds_range",
                           "scd", "sfcWind_max", "sfcWind_min", "sfcWind_mean", "sfcWind_range",
                           "swb", "swe", "vpd_max", "vpd_min", "vpd_mean", "vpd_range")) {
              var1 <- "bio"
            }
            
            if (var == "pet") var1 <- "pet_penman"
            
            if (var1 == "bio") {
              
              if (var %in% c("rsds_max", "rsds_min", "rsds_mean", "rsds_range")) {
                parts <- unlist(strsplit(var, "_"))
                part1 <- parts[1]
                part2 <- parts[2]
                filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                    part1, year_str, part2)
                url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
                canon <- var
                user_name <- paste0(var, "_", year_str)
                dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
                handle_file(url, dest_file, canon, user_name)
              } else {
                filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                                    var, year_str)
                url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
                canon <- var
                user_name <- paste0(var, "_", year_str)
                dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
                handle_file(url, dest_file, canon, user_name)
              }
              
            } else {
              
              # Determine months to download
              if (!is.null(months)) {
                months_to_download <- sprintf("%02d", months)
              } else {
                months_to_download <- sprintf("%02d", 1:12)
              }
              
              for (m in months_to_download) {
                filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                    var, m, year_str)
                url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
                canon <- var
                user_name <- paste0(var, "_", year_str, "_", m)
                dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
                handle_file(url, dest_file, canon, user_name)
              }
            }
            
          } else {
            # Future CHELSAv2
            if (is.null(gcm) || is.null(ssp)) {
              missing <- c("gcm", "ssp")[c(is.null(gcm), is.null(ssp))]
              cli::cli_abort(c(
                "CHELSA future projections (years {.val {year_str}}) require both {.arg gcm} and {.arg ssp}.",
                "x" = "Missing: {.arg {missing}}.",
                "i" = "e.g. {.code chelsa(vars = \"bio1\", years = \"2041-2070\", gcm = \"GFDL-ESM4\", ssp = 5, rcp = 8.5)}.",
                "i" = "{.arg ssp} and {.arg rcp} combine into the scenario (ssp = 5, rcp = 8.5 -> ssp585); or pass a full code like {.val 585} to {.arg ssp} alone."
              ))
            }
            # Combine ssp + rcp into the CMIP6 scenario code (e.g. ssp = 5, rcp = 8.5 -> "585").
            # When rcp is NULL, ssp is assumed to already encode the full scenario (e.g. "585").
            ssp_codes <- combine_ssp_rcp(ssp, rcp)
            for (g in gcm) {
              for (s in ssp_codes) {
                base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies"
                
                var1 <- var
                
                if (grepl("^bio\\d+$", var) | 
                    var %in% c("ai", "clt_max", "clt_min",
                               "clt_mean", "clt_range", "cmi_max", "cmi_min",
                               "cmi_mean", "cmi_range", "fcf", "fgd", "gdd0", "gdd5",
                               "gdd10", "gddlgd0", "gddlgd5", "gddlgd10",
                               "gddfgd0", "gddfgd5", "gddfgd10", "gls", "gsp", "gst",
                               "hurs_max", "hurs_min", "hurs_mean", "hurs_range",
                               "kg0", "kg1", "kg2", "kg3", "kg4", "kg5",
                               "lgd", "ngd0", "ngd5", "ngd10", "npp",
                               "pet_penman_max", "pet_penman_min", "pet_penman_mean",
                               "pet_penman_range", "rsds_max", "rsds_min", "rsds_mean", "rsds_range",
                               "scd", "sfcWind_max", "sfcWind_min", "sfcWind_mean", "sfcWind_range",
                               "swb", "swe", "vpd_max", "vpd_min", "vpd_mean", "vpd_range")) {
                  var1 <- "bio"
                }
                
                if (var == "pet") var1 <- "pet_penman"
                
                if (var1 == "bio") {
                  filename <- sprintf("CHELSA_%s_%s_%s_%s_V.2.1.tif", 
                                      var, year_str, tolower(g), paste0("ssp", s))
                  url <- sprintf("%s/%s/%s/%s/%s/%s", 
                                 base_url, year_str, toupper(g), paste0("ssp", s), var1, filename)
                  canon <- var
                  user_name <- paste0(var, "_", year_str, "_", g, "_ssp", s)
                  dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
                  handle_file(url, dest_file, canon, user_name)
                  
                } else {
                  
                  # Determine months to download
                  if (!is.null(months)) {
                    months_to_download <- sprintf("%02d", months)
                  } else {
                    months_to_download <- sprintf("%02d", 1:12)
                  }
                  
                  for (m in months_to_download) {
                    t1 <- gsub("-", "_", year_str)
                    filename <- sprintf("CHELSA_%s_r1i1p1f1_w5e5_%s_%s_%s_%s_norm.tif", 
                                        tolower(g), paste0("ssp", s), var, m, t1)
                    url <- sprintf("%s/%s/%s/%s/%s/%s", 
                                   base_url, year_str, toupper(g), paste0("ssp", s), var1, filename)
                    canon <- var
                    user_name <- paste0(var, "_", year_str, "_", m, "_", g, "_ssp", s)
                    dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
                    handle_file(url, dest_file, canon, user_name)
                  }
                }
              }
            }
          }
        }
        
        # Monthly time-series (1979 onwards)
        # NOTE: CHELSA moved its monthly archive to a new host with a per-year
        # folder structure. The old host only served files up to June 2019; the
        # new host (os.unil.cloud.switch.ch/chelsa02) serves the full record,
        # including every month after June 2019, and uses a consistent
        # CHELSA_<var>_<month>_<year>_V.2.1.tif naming (also for rsds).
        year_num <- suppressWarnings(as.numeric(year_str))
        if (!is.na(year_num) && !is_range && year_num >= 1979) {
          base_url <- "https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/monthly"

          # Map var to folder name
          var1 <- var
          if (var == "pet") var1 <- "pet_penman"

          # Determine months to download
          if (!is.null(months)) {
            months_to_download <- sprintf("%02d", months)
          } else {
            months_to_download <- sprintf("%02d", 1:12)
          }

          for (m in months_to_download) {
            filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif", var, m, year_str)
            url <- sprintf("%s/%s/%s/%s", base_url, var1, year_str, filename)
            canon <- var
            user_name <- paste0(var, "_", year_str, "_", m)
            dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
            handle_file(url, dest_file, canon, user_name)
          }
        }
        
        # Annual (swb only)
        if (var == "swb") {
          base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/annual/swb"
          filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                              var, year_str)
          url <- sprintf("%s/%s", base_url, filename)
          canon <- var
          user_name <- paste0(var, "_", year_str)
          dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))
          handle_file(url, dest_file, canon, user_name)
        }
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Return output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    # If x was already a SpatRaster (from previous function), combine
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(
          existing_stack = x,
          new_stack = processed_stack,
          current_global_extent = current_global_extent
        )
        
        if (set_na==TRUE){
          
          na_mask <- terra::app(processed_stack, anyNA)
          processed_stack <- terra::mask(processed_stack, na_mask, maskvalues = TRUE)
          
        }
        
        
      } else {
        # Regional mode: resample new layers to match input raster exactly
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = choose_resample_method(processed_stack))
          
        }
        processed_stack <- c(x, processed_stack)
        # Remove NAs if necessary
        if (set_na==TRUE){
          
          na_mask <- terra::app(processed_stack, anyNA)
          processed_stack <- terra::mask(processed_stack, na_mask, maskvalues = TRUE)
          
        }
      }
      
      
    }
    
    # Attach global extent as attribute for downstream functions
    if (is_global) {
      
      if (land == TRUE){
        cli::cli_alert_info(paste0(
          "Global masking with land boundary from Natural Earth database...\n",
          "Website: {.url https://www.naturalearthdata.com/}\n"
        ))
        invisible(capture.output(suppressMessages(suppressWarnings(land_sf <- rnaturalearth::ne_download(
          scale = "medium",
          type = "land",
          category = "physical",
          returnclass = "sf")))))
        
        processed_stack <-terra::crop(terra::mask(processed_stack, land_sf), land_sf)
      }
      
      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
      
    }
    
    attr(processed_stack, "set_na") <- set_na
    attr(processed_stack, "path") <- path
    attr(processed_stack, "land") <- land
    
    # Remove NAs if necessary
    if (set_na==TRUE){
      
      cli::cli_alert_info("Applying NA mask...")
      
      master_mask <- sum(processed_stack)
      # Apply that master mask to the whole stack
      processed_stack <- terra::mask(processed_stack, master_mask)
      
    }
    
    if (!is.null(path)){
      terra::writeRaster(processed_stack, path, overwrite = TRUE)
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    if (inherits(x, "data.frame") && !inherits(x, "sf")) {
      extracted_df <- merge(x, extracted_df[, c(1, 4:ncol(extracted_df))], by = c("ID"), all = TRUE)
      # Preserve CRS from previous extraction
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) {
        crs <- prev_crs
      }
    }
    
    # Store the CRS as an attribute for downstream functions
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path
    
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}