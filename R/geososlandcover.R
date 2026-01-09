# R/geososlandcover.R

#' Download and process Global Simulation Land Use/Cover (GEOSOS) 1 km variables
#'
#' This function downloads, processes, and extracts variables from the
#' Global Land-Use and Land-Cover Change Product (2010-2100). The dataset 
#' provides global 1 km resolution rasters based on different IPCC scenarios.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Land Cover Classification}
#' \itemize{
#'   \item 1 - "landcover" (Categorical: 1=Water, 2=Forest, 3=Grassland, 4=Farmland, 5=Urban, 6=Barren) ("lc", "cover", "land cover", "land cover class", "land use", "lulc", "classes")
#' }
#'
#' \strong{Simulation Parameters}
#' \itemize{
#'   \item \strong{Years:} 2010, 2050, 2100 (Note: 2010 is the MODIS baseline).
#'   \item \strong{Scenarios:} "A1B", "A2", "B1", "B2" (Ignored if year is 2010).
#' }
#'
#' \strong{Citation:}\cr
#' Xia Li, Guangzhao Chen, Xiaoping Liu, Xun Liang, Shaojian Wang, Yimin Chen, 
#' Fengsong Pei & Xiaocong Xu (2017). A New Global Land-Use and Land-Cover Change 
#' Product at a 1-km Resolution for 2010 to 2100 Based on Human–Environment 
#' Interactions. Annals of the American Association of Geographers, 107:5, 
#' 1040-1059.
#' DOI: \url{https://doi.org/10.1080/24694452.2017.1303357}
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param scenario Character. The IPCC scenario: "A1B", "A2", "B1", or "B2". Ignored if year is 2010.
#' @param year Numeric or character. The year of the product: 2010, 2050, or 2100.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Example 1: Download land cover for Italy in 2050 under scenario A1B
#' processed <- var_get(country = "Italy", crs = 3035) %>% 
#'   geososlandcover(vars = c("land cover"), year = 2050, scenario = "A1B")
#'   
#' # Example 2: Extract baseline (2010) values for specific points
#' points_df <- data.frame(ID = 1:2, x = c(12, 13), y = c(42, 43))
#' extracted <- var_get(data = points_df, crs = 4326) %>%
#'   geososlandcover(vars = "lc", year = 2010)
#' }
#' @export

geososlandcover <- function(x, vars, scenario = "A1B", year = 2010, discover = TRUE, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Simulation Land Use/Cover (GEOSOS) variables.\n",
    "Citation: Xia Li et al. (2017) A New Global Land-Use and Land-Cover Change Product at a 1-km Resolution for 2010 to 2100. Annals of the American Association of Geographers.\n",
    "DOI: {.url https://doi.org/10.1080/24694452.2017.1303357}\n"
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
  # Argument Validation and URL Construction
  # --------------------------------------------------------------------
  year <- as.numeric(year)
  valid_years <- c(2010, 2050, 2100)
  
  if (!(year %in% valid_years)) {
    cli::cli_abort("Invalid year. Choose from: {.val {valid_years}}")
  }
  
  if (year == 2010) {
    # No scenario logic for 2010
    url_to_use <- "http://geosimulation.cn/download/GlobalSimulation/Global_MODIS_2010/MODISLandcover_2010_reclass_1km.tif"
    label_suffix <- "2010"
  } else {
    # Scenario is required for future years
    scenario <- toupper(as.character(scenario))
    valid_scenarios <- c("A1B", "A2", "B1", "B2")
    if (!(scenario %in% valid_scenarios)) {
      cli::cli_abort("Scenario {.val {scenario}} is invalid for year {.val {year}}. Choose from: {.val {valid_scenarios}}")
    }
    url_to_use <- paste0(
      "http://geosimulation.cn/download/GlobalSimulation/world_", scenario, "_", year, "/",
      "world_", scenario, "_", year, ".tif"
    )
    label_suffix <- paste0(scenario, "_", year)
  }
  
  # --------------------------------------------------------------------
  # Friendly-name mapping
  # --------------------------------------------------------------------
  geosos_lookup <- list(
    "landcover" = c("lc", "cover", "land cover", "land cover class", "land use", "lulc", "classes")
  )
  
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  syn2canon <- list()
  for (canon in names(geosos_lookup)) {
    for (syn in geosos_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  requested_codes <- character(0)
  code_to_user_name <- list() 
  unmapped <- character(0)
  
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      canon <- syn2canon[[key]]
      if (!(canon %in% requested_codes)) {
        requested_codes <- c(requested_codes, canon)
        code_to_user_name[[canon]] <- v
      }
    } else {
      unmapped <- c(unmapped, v)
    }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c("Unknown variables:", "x" = "{.val {unmapped}}"))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download and Process
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    msg_label <- if(year == 2010) "year 2010" else paste(scenario, year)
    cli::cli_alert_info("Downloading data for {.val {user_name}} ({msg_label})...")
    
    success <- download_file(url, dest_file)
    
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
       # if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      # --------------------------------------------------------------------
      # GEOSOS data is in LAEA projection (EPSG:3857-like with meters)
      # Must reproject to target CRS BEFORE processing
      # --------------------------------------------------------------------
      source_crs <- terra::crs(layer)
      target_crs <- crs
      
      # Check if source CRS differs from target CRS
      source_crs_sf <- tryCatch(sf::st_crs(source_crs), error = function(e) NULL)
      target_crs_sf <- tryCatch(sf::st_crs(target_crs), error = function(e) NULL)
      
      crs_match <- FALSE
      if (!is.null(source_crs_sf) && !is.null(target_crs_sf)) {
        crs_match <- isTRUE(source_crs_sf == target_crs_sf)
      }
      
      if (!crs_match) {
        cli::cli_alert_info("Source data is in different CRS. Reprojecting to target CRS...")
        # Use "near" method for categorical land cover data
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
      
      names(layer1) <- paste0(user_name, "_", label_suffix)
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {names(layer1)}} to stack.")
      rm(layer, layer1); gc()
      #if (!is_global) fs::file_delete(dest_file)
      
    } else {
      # Point extraction mode
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      # For point extraction, we need to handle the CRS mismatch
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        #if (!is_global) fs::file_delete(dest_file)
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
        # Write temporary reprojected file for process_points
        temp_reproj <- file.path(fs::path_temp("envar/grids"), paste0("reproj_", basename(dest_file)))
        terra::writeRaster(layer, temp_reproj, overwrite = TRUE)
        dest_file_for_extract <- temp_reproj
      } else {
        dest_file_for_extract <- dest_file
      }
      
      extracted <- try(process_points(file = dest_file_for_extract, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        #if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
        names(extracted)[ncol(extracted)] <- paste0(user_name, "_", label_suffix)
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      rm(extracted); gc()
      #if (!is_global) fs::file_delete(dest_file)
      # Clean up temporary reprojected file if created
      if (exists("temp_reproj") && fs::file_exists(temp_reproj)) {
        #fs::file_delete(temp_reproj)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Execution
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of GEOSOS Land Cover data...")
  
  for (canon in requested_codes) {
    filename <- paste0("geosos_", label_suffix, ".tif")
    user_name <- code_to_user_name[[canon]]
    dest <- file.path(fs::path_temp("envar/grids"), paste0(user_name, ".tif"))
    
    handle_file(url_to_use, dest, canon, user_name)
  }
  
  # --------------------------------------------------------------------
  # Return Output (Raster/Point logic same as example)
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(x, processed_stack, current_global_extent)
      } else {
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = "near")
        }
        processed_stack <- c(x, processed_stack)
      }
    }
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
    attr(processed_stack, "land")<-land
    if (set_na) {
      cli::cli_alert_info("Applying NA mask...")
      master_mask <- sum(processed_stack)
      processed_stack <- terra::mask(processed_stack, master_mask)
    }
    if (!is.null(path)) terra::writeRaster(processed_stack, path, overwrite = TRUE)
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
    if (!is.null(path)) write.csv(extracted_df, path)
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}