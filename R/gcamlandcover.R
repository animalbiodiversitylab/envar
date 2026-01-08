# R/gcamlandcover.R

#' Download and process Global Future Land Use/Cover layers (2020-2100)
#'
#' This function downloads, processes, and extracts simulated global land use 
#' and land cover (LULC) data for the period 2020-2100.
#'
#' @details
#' The data represents 1 km resolution LULC maps. The original data is in 
#' World Mercator projection and will be automatically reprojected to the 
#' CRS defined in `var_get()`.
#'
#' \strong{Land cover codes}
#' \itemize{
#'   \item 1 - Cropland
#'   \item 2 - Forest
#'   \item 3 - Grassland
#'   \item 4 - Urban
#'   \item 5 - Barren
#'   \item 6 - Water
#' }
#'
#' \strong{Available Years}
#' \itemize{
#'   \item 2020, 2030, 2050, 2070, 2100
#' }
#'
#' \strong{Available SSPs (Shared Socioeconomic Pathways)}
#' \itemize{
#'   \item 126 (SSP1-2.6)
#'   \item 245 (SSP2-4.5)
#'   \item 370 (SSP3-7.0)
#'   \item 434 (SSP4-3.4)
#'   \item 585 (SSP5-8.5)
#' }
#' 
#' \strong{Citation:}\cr
#' Zhang T, Cheng C, Wu X (2023). "Mapping the spatial heterogeneity of global land use and land cover from 2020 to 2100 at a 1 km resolution." Scientific Data 10, 748.
#' https://doi.org/10.1038/s41597-023-02637-7
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' @param vars Character. Currently unused/ignored as this function returns the 
#' landcover map defined by `year` and `ssp`, but kept for consistency. 
#' Default is "landcover".
#' @param ssp Numeric or Character. The SSP scenario code (126, 245, 370, 434, 585). 
#' Ignored if `year` is 2020.
#' @param year Numeric. The year of simulation (2020, 2030, 2050, 2070, 2100).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack.
#' If `var_get()` contained spatial points without buffer: a `data.frame`.
#'
#' @examples
#' \dontrun{
#' # Get Baseline (2020)
#' processed <- var_get(country= "Italy", crs=4326) %>% 
#'   gcamlandcover(year = 2020)
#'
#' # Get Future (SSP5-8.5 in 2050)
#' processed <- var_get(country= "Italy", crs=4326) %>% 
#'   gcamlandcover(ssp = 585, year = 2050)
#' }
#' @export

gcamlandcover <- function(x, vars = "landcover", ssp = 126, year = 2020, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Future Land Use/Cover layers (2020-2100).\n",
    "Citation: Zhang T, Cheng C, Wu X (2023). Mapping the spatial heterogeneity of global land use and land cover from 2020 to 2100 at a 1 km resolution. Scientific Data 10, 748.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-023-02637-7}\n"
  ))
  
  par_list <- get_par(x)
  
  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res  <- par_list$res
    crs  <- par_list$crs
    is_global <- isTRUE(par_list$is_global)
    is_raster_input <- TRUE
    set_na = par_list$set_na
    path = par_list$path
    current_global_extent <- par_list$global_extent
    land = par_list$land
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs  <- par_list$crs
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
  # Argument Validation and URL Selection
  # --------------------------------------------------------------------
  year <- as.numeric(year)
  valid_years <- c(2020, 2030, 2050, 2070, 2100)
  
  if (!(year %in% valid_years)) {
    cli::cli_abort("Invalid year. Supported years: {.val {valid_years}}")
  }
  
  # URL Lookup Table
  # Maps "ssp_year" key to file ID
  url_map <- list(
    # Baseline
    "2020" = "41355606",
    
    # SSP1-2.6 (126)
    "126_2030" = "41355609", "126_2050" = "41355612", "126_2070" = "41355615", "126_2100" = "41355618",
    
    # SSP2-4.5 (245)
    "245_2030" = "41355621", "245_2050" = "41355624", "245_2070" = "41355627", "245_2100" = "41355630",
    
    # SSP3-7.0 (370)
    "370_2030" = "41355633", "370_2050" = "41355636", "370_2070" = "41355639", "370_2100" = "41355642",
    
    # SSP4-3.4 (434)
    "434_2030" = "41355645", "434_2050" = "41355648", "434_2070" = "41355651", "434_2100" = "41355654",
    
    # SSP5-8.5 (585)
    "585_2030" = "41355657", "585_2050" = "41355660", "585_2070" = "41355663", "585_2100" = "41355666"
  )
  
  # Determine Key and Label
  if (year == 2020) {
    lookup_key <- "2020"
    label_name <- "landcover_2020"
    file_label <- "2020"
  } else {
    # Validate SSP
    ssp <- as.character(ssp)
    valid_ssps <- c("126", "245", "370", "434", "585")
    if (!(ssp %in% valid_ssps)) {
      cli::cli_abort("Invalid SSP code {.val {ssp}}. Supported: {.val {valid_ssps}}")
    }
    lookup_key <- paste0(ssp, "_", year)
    label_name <- paste0("landcover_ssp", ssp, "_", year)
    file_label <- paste0("ssp", ssp, "_", year)
  }
  
  file_id <- url_map[[lookup_key]]
  if (is.null(file_id)) {
    cli::cli_abort("Could not find download URL for configuration: Year {.val {year}}, SSP {.val {ssp}}")
  }
  
  final_url <- paste0("https://figshare.com/ndownloader/files/", file_id)
  
  # --------------------------------------------------------------------
  # Helper: Download, Reproject, and Process
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, user_name) {
    temp_dir <- fs::path_temp("envar/futurelandcover")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {user_name}}...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download from {.url {url}}.")
      return(NULL)
    }
    
    # ----------------------------------------------------------------
    # RASTER PROCESSING
    # ----------------------------------------------------------------
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      # Check and Reproject CRS if necessary
      source_crs <- terra::crs(layer)
      target_crs <- crs
      
      source_crs_sf <- tryCatch(sf::st_crs(source_crs), error = function(e) NULL)
      target_crs_sf <- tryCatch(sf::st_crs(target_crs), error = function(e) NULL)
      
      crs_match <- FALSE
      if (!is.null(source_crs_sf) && !is.null(target_crs_sf)) {
        crs_match <- isTRUE(source_crs_sf == target_crs_sf)
      }
      
      if (!crs_match) {
        cli::cli_alert_info("Source data (World Mercator) differs from target CRS. Reprojecting...")
        # Use "near" (Nearest Neighbor) for categorical land cover data
        layer <- terra::project(layer, target_crs, method = "near")
      }
      
      cli::cli_alert_info("Processing layer {.val {user_name}}...")
      
      result <- process_raster_layer(
        layer = layer, grid = grid, mask = mask, res = res, crs = crs,
        is_global = is_global, current_extent = current_global_extent
      )
      
      if (is_global) {
        layer1 <- result$layer
        new_extent <- result$extent
        
        current_global_extent <<- new_extent
        if (!is.null(processed_stack)) {
          processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        layer1 <- result
      }
      
      names(layer1) <- user_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      rm(layer, layer1); gc()
      if (!is_global) fs::file_delete(dest_file)
      
      # ----------------------------------------------------------------
      # POINT EXTRACTION
      # ----------------------------------------------------------------
    } else {
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      # Reproject to target CRS before extraction
      source_crs <- terra::crs(layer)
      target_crs <- crs
      
      source_crs_sf <- tryCatch(sf::st_crs(source_crs), error = function(e) NULL)
      target_crs_sf <- tryCatch(sf::st_crs(target_crs), error = function(e) NULL)
      
      crs_match <- FALSE
      if (!is.null(source_crs_sf) && !is.null(target_crs_sf)) {
        crs_match <- isTRUE(source_crs_sf == target_crs_sf)
      }
      
      if (!crs_match) {
        cli::cli_alert_info("Reprojecting layer to match points CRS...")
        layer <- terra::project(layer, target_crs, method = "near")
        # Write temporary reprojected file
        temp_reproj <- file.path(fs::path_temp("envar/futurelandcover"), paste0("reproj_", basename(dest_file)))
        terra::writeRaster(layer, temp_reproj, overwrite = TRUE)
        dest_file_for_extract <- temp_reproj
      } else {
        dest_file_for_extract <- dest_file
      }
      
      extracted <- try(process_points(file = dest_file_for_extract, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
        names(extracted)[ncol(extracted)] <- user_name
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      rm(extracted); gc()
      if (!is_global) fs::file_delete(dest_file)
      # Clean up temporary reprojected file if created
      if (exists("temp_reproj") && fs::file_exists(temp_reproj)) {
        fs::file_delete(temp_reproj)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Execute Download and Processing
  # --------------------------------------------------------------------
  filename <- paste0(file_label, ".tif")
  dest <- file.path(fs::path_temp("envar/futurelandcover"), filename)
  
  handle_file(final_url, dest, label_name)
  
  # --------------------------------------------------------------------
  # Return Output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(x, processed_stack, current_global_extent)
      } else {
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          # Categorical data requires 'near' method
          processed_stack <- terra::resample(processed_stack, x, method = "near")
        }
        processed_stack <- c(x, processed_stack)
      }
    }
    
    if (is_global) {
      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
    }
    
    attr(processed_stack, "set_na") <- set_na
    attr(processed_stack, "path") <- path
    attr(processed_stack, "land")<- land
    
    if (set_na) {
      cli::cli_alert_info("Applying NA mask...")
      master_mask <- sum(processed_stack)
      processed_stack <- terra::mask(processed_stack, master_mask)
    }
    
    if (!is.null(path)) {
      terra::writeRaster(processed_stack, path, overwrite = TRUE)
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    
    if (inherits(x, "data.frame") && !inherits(x, "sf")) {
      extracted_df <- merge(x, extracted_df[, c(1, 4:ncol(extracted_df))], by = c("ID"), all = TRUE)
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) crs <- prev_crs
    }
    
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path
    
    if (!is.null(path)) {
      write.csv(extracted_df, path)
    }
    
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}