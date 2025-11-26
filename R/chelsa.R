# R/chelsa.R

#' Download CHELSA climate data
#'
#' This function builds URLs and downloads CHELSA climate data based on
#' variables, years, months, climate models, and scenarios.
#' Uses the helper function `download_file` to handle downloads.
#' Each raster layer is downloaded, processed (cropped/masked/resampled),
#' added to the output stack immediately, and temporary files are deleted
#' to minimize memory use.
#'
#' @param x A SpatRaster or sf object to define the area of interest
#' @param vars A character vector of variables to download (e.g., "tas", "pr", "bio")
#' @param years A character or numeric vector of years or year ranges (e.g., "1981-2010", 2015)
#' @param months A numeric vector (1–12) specifying which months to download.
#'        If NULL and `years` are single years, all 12 months are downloaded.
#' @param gcm General Circulation Model(s) for future projections
#' @param rcp Representative Concentration Pathway for CMIP5 data
#' @param ssp Shared Socioeconomic Pathway for CMIP6 data
#' @param cruts_years Numeric vector. Years to download from CHELSAcruts (must be 1901–2016)
#' @param ... Additional arguments (currently unused)
#'
#' @return A SpatRaster stack or data.frame with extracted values

chelsa <- function(x, vars, years = NULL, months = NULL, gcm = NULL, rcp = NULL, 
                   ssp = NULL, cruts_years = NULL, ...) {
  
  old_timeout <- getOption("timeout")
  options(timeout=max(100000000000000,old_timeout))
  on.exit(options(timeout=old_timeout))
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using CHELSA.\n",
    "Citation: Karger, D. E., et al. (2017). Climatologies at high resolution for the earth’s land surface areas. Scientific Data\n",
    "DOI: {.url https://doi.org/10.1038/sdata.2017.122}\n"
  ))
  
  cli::cli_alert_info("Starting the download of CHELSA data...")
  
  par_list <- get_par(x)
  
  if (inherits(par_list[[1]], "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res  <- par_list$res
    is_raster_input <- TRUE
  } else {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    is_raster_input <- FALSE
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var, m = NULL, y = NULL) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    
    success <- download_file(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {var}} ({.val {m}}/{.val {y}}) from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning(" Could not read raster {.val {dest_file}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {basename(dest_file)}}...")
      
      layer <- terra::crop(layer, grid, snap = "out")
      layer <- terra::resample(layer, grid, method = "bilinear")
      layer <- terra::mask(layer, mask)
      
      if (!is.null(par_list$crs)) {
        layer <- terra::project(layer, par_list$crs)
      }
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer
      } else {
        processed_stack <<- c(processed_stack, layer)
      }
      
      cli::cli_alert_success("Processed and added {.val {basename(dest_file)}} to stack.")
      
      rm(layer)
      gc()
      fs::file_delete(dest_file)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {basename(dest_file)}}...")
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning(" Extraction failed for {.val {basename(dest_file)}}.")
        fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, 4)], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {basename(dest_file)}} successfully.")
      
      rm(extracted)
      gc()
      fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # --- Handle CHELSAcruts (1901–2016) ---
  # --------------------------------------------------------------------
  if (!is.null(cruts_years)) {
    if (any(cruts_years < 1901) || any(cruts_years > 2016)) {
      cli::cli_abort("CHELSAcruts data is only available for years 1901–2016.")
    }
    
    # valid_cruts_vars <- c("pr", "tasmax", "tasmin")
    # invalid_vars <- setdiff(vars, valid_cruts_vars)
    # if (length(invalid_vars) > 0) {
    #   cli::cli_abort("CHELSAcruts only supports variables: {.val {paste(valid_cruts_vars, collapse=', ')}}. Invalid: {.val {paste(invalid_vars, collapse=', ')}}")
    # }
    
    cli::cli_alert_info("Starting the download of CHELSAcruts data...")
    base_url <- "https://os.zhdk.cloud.switch.ch/chelsav1/chelsa_cruts"
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
          url <- sprintf("%s/%s/%s", base_url, var_cruts, filename)
          dest_file <- file.path(fs::path_temp("envar/grids"), filename)
          handle_file(url, dest_file, var, month, year)
        }
      }
    }
  }
  
  # --------------------------------------------------------------------
  # --- Standard CHELSA (v1/v2) Processing ---
  # --------------------------------------------------------------------
  if (!is.null(years) && length(years) > 0) {
    
    if ("bio" %in% vars) {
      bio_range <- readline(prompt = "Enter BIO numbers (e.g., 1:19 or 3:5): ")
      bio_nums <- eval(parse(text = bio_range))
      vars <- setdiff(vars, "bio")
      vars <- c(vars, paste0("bio", bio_nums))
    }
    
    for (var in vars) {
      for (y in years) {
        year_str <- as.character(y)
        is_range <- grepl("-", year_str)
        
        ## ---- Historical CMIP5 (chelsav1) ----
        if (year_str %in% c("2041-2060", "2061-2080")) {
          for (g in gcm) {
            for (r in rcp) {
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
              
              filename <- sprintf("CHELSA_%s_mon_%s_rcp%d_r1i1p1_g025.nc_%s%s.tif",
                                  var, g, r, year_str, version_suffix)
              
              url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
              dest_file <- file.path(fs::path_temp("envar/grids"), filename)
              handle_file(url, dest_file, var, NULL, y)
            }
          }
        }
        
        ## ---- CHELSAv2 Climatologies ----
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
                dest_file <- file.path(fs::path_temp("envar/grids"), filename)
                handle_file(url, dest_file, var, NULL, y)
              } else {
                filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                                    var, year_str)
                url <- sprintf("%s/%s/%s/%s", base_url, year_str, var1, filename)
                dest_file <- file.path(fs::path_temp("envar/grids"), filename)
                handle_file(url, dest_file, var, NULL, y)
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
                dest_file <- file.path(fs::path_temp("envar/grids"), filename)
                handle_file(url, dest_file, var, m, y)
              }
            }
            
          } else {
            ## ---- Future CHELSAv2 ----
            for (g in gcm) {
              for (s in ssp) {
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
                  dest_file <- file.path(fs::path_temp("envar/grids"), filename)
                  handle_file(url, dest_file, var, NULL, y)
                  
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
                    dest_file <- file.path(fs::path_temp("envar/grids"), filename)
                    handle_file(url, dest_file, var, m, y)
                  }
                }
              }
            }
          }
        }
        
        ## ---- Monthly (1979-2019) ----
        if (year_str %in% as.character(1979:2019)) {
          base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly"
          
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
            if (var == "rsds") {
              filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                  var, year_str, m)
            } else {
              filename <- sprintf("CHELSA_%s_%s_%s_V.2.1.tif",
                                  var, m, year_str)
            }
            url <- sprintf("%s/%s/%s", base_url, var1, filename)
            dest_file <- file.path(fs::path_temp("envar/grids"), filename)
            handle_file(url, dest_file, var, m, y)
          }
        }
        
        ## ---- Annual (swb only) ----
        if (var == "swb") {
          base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/annual/swb"
          filename <- sprintf("CHELSA_%s_%s_V.2.1.tif",
                              var, year_str)
          url <- sprintf("%s/%s", base_url, filename)
          dest_file <- file.path(fs::path_temp("envar/grids"), filename)
          handle_file(url, dest_file, var, NULL, y)
        }
      }
    }
  }
  
  # --------------------------------------------------------------------
  # --- Return processed results ---
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    if (inherits(x, "SpatRaster")) processed_stack <- c(x, processed_stack)
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}