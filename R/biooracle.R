# R/biooracle.R

#' Download Bio-ORACLE marine data
#'
#' This function downloads, processes, and extracts variables from the
#' Bio-ORACLE v3.0 dataset.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses and ""):
#' 
#' \itemize{
#'   \item 1 - "thetao" (Ocean temperature; [ºC]) ("temperature", "temp", "sea temperature")
#'   \item 2 - "so" (Salinity; [-]) ("sal", "salt", "saltiness")
#'   \item 3 - "sws" (Sea water velocity; [m.s-1]) ("velocity", "current speed", "speed")
#'   \item 4 - "swd" (Sea water direction; [degree]) ("direction", "current direction")
#'   \item 5 - "no3" (Nitrate; [mmol . m-3]) ("nitrate")
#'   \item 6 - "po4" (Phosphate; [mmol . m-3]) ("phosphate")
#'   \item 7 - "si" (Silicate; [mmol . m-3]) ("silicate", "silicon")
#'   \item 8 - "o2" (Dissolved molecular oxygen; [mmol . m-3]) ("oxygen", "dissolved oxygen", "o2")
#'   \item 9 - "dfe" (Iron; [mmol . m-3]) ("iron", "fe")
#'   \item 10 - "phyc" (Primary productivity; [mmol . m-3]) ("productivity", "pp", "primary production")
#'   \item 11 - "ph" (pH; [-]) ("acidity")
#'   \item 12 - "chl" (Chlorophyll; [mg . m-3]) ("chlorophyll", "chla")
#'   \item 13 - "sithick" (Sea ice thickness; [m]) ("ice thickness")
#'   \item 14 - "siconc" (Sea ice cover; [Fraction]) ("ice cover", "sea ice")
#'   \item 15 - "clt" (Cloud cover; [%]) ("cloud", "clouds")
#'   \item 16 - "mlotst" (Mixed layer depth; [m]) ("mld", "mixed layer")
#'   \item 17 - "tas" (Air temperature; [ºC]) ("air temperature", "air temp")
#'   \item 18 - "par" (Photosynt. Avail. Radiation; [E.m-2.day-1]) ("light", "radiation")
#'   \item 19 - "kdpar" (Diffuse attenuation; [m-1]) ("attenuation", "turbidity")
#'   \item 20 - "bathymetry" (Bathymetry; [m]) ("depth", "elevation", "altitude")
#'   \item 21 - "slope" (Topographic slope; [-]) ("topographic slope")
#'   \item 22 - "aspect" (Topographic aspect; [-]) ("topographic aspect")
#'   \item 23 - "tpi" (Topographic position index; [-]) ("topographic position index")
#'   \item 24 - "tri" (Terrain ruggedness index; [-]) ("terrain ruggedness index", "ruggedness")
#' }
#'
#' \strong{Citation:}\cr
#' Assis J, Fernández Bejarano SJ, Salazar VW, Schepers L, Gouvêa L, Fragkopoulou E, Leclercq F, Vanhoorne B, Tyberghein L, Serrão EA, Verbruggen H, De Clerck O (2024). "Bio-ORACLE v3.0. Pushing marine data layers to the CMIP6 Earth system models of climate change research." Global Ecology and Biogeography, 33, e13813.
#' https://doi.org/10.1111/geb.13813
#'
#' @section Resolution:
#' Bio-ORACLE layers are distributed on a 0.05-degree grid (~5.5 km at the
#' equator). Because \code{res} is a multiplier of the 30 arc-second base grid,
#' the value that reproduces this grid exactly is \code{res = 6}
#' (\eqn{6 \times 30''= 0.05^{\circ}}). You must therefore call \code{par_set()}
#' with \code{res = 6}; any other value (including the default) raises an error.
#'
#' @param x The output from `par_set()` defining the area or locations. It must
#'   have been created with `res = 6` (Bio-ORACLE's native 0.05-degree grid).
#' @param vars Character vector of one or more variables or synonyms to download.
#' @param realm Character. One of "surface" (default), "benthic_minimum", "benthic_average", or "benthic_maximum".
#' @param years Character. The time period for the data in "YYYY-YYYY" format. 
#' Use "2000-2010" or "2010-2020" for baseline current conditions (default is "2000-2010"). 
#' For future projections, specify the decade (e.g., "2040-2050", "2090-2100") and provide the `ssp` argument.
#' @param ssp numeric or character. Shared Socioeconomic Pathway (119, 126, 245, 370, 460, 585). Required if `years` is in the future (>= 2020).
#' @param algorithm Character. Statistic to apply (max, mean, min, ltmax, ltmin, range). Default "mean".
#' @param ... Additional arguments.
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Example 1: Current conditions (Baseline)
#' current_env <- par_set(country = "Italy", crs = 3035, res = 6) %>%
#'   biooracle(vars = c("temperature", "salinity"),
#'             years = "2000-2010")
#'
#' # Example 2: Future projections (2050, SSP 585)
#' future_env <- par_set(country = "Italy", crs = 3035, res = 6) %>%
#'   biooracle(vars = c("temperature", "salinity"),
#'             years = "2040-2050",
#'             ssp = 585)
#'   }
#' @export

biooracle <- function(x, vars, realm = "surface", years = "2000-2010", 
                      ssp = NULL, algorithm = "mean", ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Bio-ORACLE data.\n",
    "Citation: Assis J, Fern\u00e1ndez Bejarano SJ, Salazar VW, Schepers L, Gouv\u00eaa L, Fragkopoulou E, Leclercq F, Vanhoorne B, Tyberghein L, Serr\u00e3o EA, Verbruggen H, De Clerck O (2024). Bio-ORACLE v3.0. Pushing marine data layers to the CMIP6 Earth system models of climate change research. Global Ecology and Biogeography, 33, e13813.\n",
    "DOI: {.url https://doi.org/10.1111/geb.13813}"
  ))
  
  par_list <- get_par(x)

  # --------------------------------------------------------------------
  # Enforce Bio-ORACLE native resolution
  # --------------------------------------------------------------------
  # Bio-ORACLE layers are distributed on a 0.05-degree grid (~5.5 km at the
  # equator). Since `res` is a multiplier of the 30 arc-second base grid, the
  # value that reproduces 0.05 degrees exactly is 6 (6 * 30" = 0.05 degrees).
  # Any other value would resample to a non-native grid (and anything finer
  # would only invent detail the data does not contain), so we require res = 6
  # and abort otherwise.
  if (is.null(par_list$res) || !isTRUE(all.equal(as.numeric(par_list$res), 6))) {
    supplied <- if (is.null(par_list$res)) "NULL" else as.character(par_list$res)
    cli::cli_abort(c(
      "Bio-ORACLE requires {.code res = 6} in {.fn par_set}.",
      "x" = "You supplied {.code res = {supplied}}.",
      "i" = "Bio-ORACLE layers use a 0.05-degree grid (~5.5 km at the equator), which is exactly 6 times the 30 arc-second base grid. Set {.code res = 6} in {.fn par_set}."
    ))
  }

  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid; mask <- par_list$mask; res <- par_list$res; crs <- par_list$crs
    is_global <- isTRUE(par_list$is_global); is_raster_input <- TRUE
    set_na = par_list$set_na; path = par_list$path
    current_global_extent <- par_list$global_extent
  } else if (par_list$type == "point") {
    points <- par_list$mask; bbox_points <- par_list$bbox; crs <- par_list$crs
    is_global <- FALSE; is_raster_input <- FALSE
    current_global_extent <- NULL; path = par_list$path
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  oracle_lookup <- list(
    "thetao"     = c("temperature", "temp", "sea temperature", "ocean temperature"),
    "so"         = c("sal", "salt", "saltiness", "salinity"),
    "sws"        = c("velocity", "current speed", "speed", "sea water velocity"),
    "swd"        = c("direction", "current direction", "sea water direction"),
    "no3"        = c("nitrate"),
    "po4"        = c("phosphate"),
    "si"         = c("silicate", "silicon"),
    "o2"         = c("oxygen", "dissolved oxygen", "o2", "dissolved molecular oxygen"),
    "dfe"        = c("iron", "fe"),
    "phyc"       = c("productivity", "pp", "primary production", "primary productivity"),
    "ph"         = c("acidity", "ph"),
    "chl"        = c("chlorophyll", "chla"),
    "sithick"    = c("ice thickness", "sea ice thickness"),
    "siconc"     = c("ice cover", "sea ice", "sea ice cover"),
    "clt"        = c("cloud", "clouds", "cloud cover"),
    "mlotst"     = c("mld", "mixed layer", "mixed layer depth"),
    "tas"        = c("air temperature", "air temp"),
    "par"        = c("light", "radiation", "photosynt avail radiation"),
    "kdpar"      = c("attenuation", "turbidity", "diffuse attenuation"),
    "bathymetry" = c("depth", "elevation", "altitude", "bathymetry max", "bathymetry min"),
    "slope"      = c("topographic slope"),
    "aspect"     = c("topographic aspect"),
    "tpi"        = c("topographic position index"),
    "tri"        = c("terrain ruggedness index", "ruggedness")
  )
  
  normalize_string <- function(s) {
    s <- tolower(s); s <- gsub("[[:punct:]]", " ", s); s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  syn2canon <- list()
  for (canon in names(oracle_lookup)) {
    for (syn in oracle_lookup[[canon]]) { syn2canon[[normalize_string(syn)]] <- canon }
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
        requested_codes <- c(requested_codes, canon); code_to_user_name[[canon]] <- v
      }
    } else { unmapped <- c(unmapped, v) }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c("Unknown Bio-ORACLE variables:", "x" = "{.val {unmapped}}"))
  }
  
  # --------------------------------------------------------------------
  # Helper: Handle file (Standard Structure)
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
        #if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      cli::cli_alert_info("Processing layer {.val {user_name}}...")
      result <- process_raster_layer(layer = layer, grid = grid, mask = mask, res = res, 
                                     crs = crs, is_global = is_global, current_extent = current_global_extent)
      
      if (is_global) {
        layer1 <- result$layer; new_extent <- result$extent
        current_global_extent <<- new_extent
        if (!is.null(processed_stack)) processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
      } else { layer1 <- result }
      
      names(layer1) <- user_name
      if (is.null(processed_stack)) processed_stack <<- layer1 else processed_stack <<- c(processed_stack, layer1)
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      rm(layer, layer1); gc()
      #if (!is_global) fs::file_delete(dest_file)
      
    } else {
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) #fs::file_delete(dest_file)
        return(NULL)
      }
      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) names(extracted)[ncol(extracted)] <- user_name
      if (is.null(extracted_df)) extracted_df <<- extracted else extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      rm(extracted); gc()
      #if (!is_global) fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # Loop and URL Logic
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of Bio-ORACLE data...")
  
  for (canon in requested_codes) {
    depth_tag <- switch(realm, "surface"="depthsurf", "benthic_minimum"="depthmin", "benthic_average"="depthmean", "benthic_maximum"="depthmax", "depthsurf")
    start_year <- as.numeric(substr(years, 1, 4))
    is_terrain <- canon %in% c("bathymetry", "slope", "aspect", "tpi", "tri")
    
    if (is_terrain) {
      dataset_id <- "terrain_characteristics"
      time_query <- "(1970-01-01T00:00:00Z):1:(1970-01-01T00:00:00Z)"
      # Mapping codes to ERDDAP terrain variable names
      var_map_terr <- list("bathymetry"="bathymetry", "slope"="slope", "aspect"="aspect", "tpi"="topographic_position_index", "tri"="terrain_ruggedness_index")
      var_call <- if (canon == "bathymetry") paste0("bathymetry_", algorithm) else var_map_terr[[canon]]
    } else if (start_year < 2020) {
      end_map <- list("thetao"="2019", "so"="2019", "sws"="2019", "swd"="2019", "mlotst"="2019", "chl"="2018", "no3"="2018", "po4"="2018", "o2"="2018", "dfe"="2018", "ph"="2018", "phyc"="2020", "siconc"="2020", "sithick"="2020", "clt"="2020", "tas"="2020", "par"="2020", "kdpar"="2020")
      end_yr <- if (!is.null(end_map[[canon]])) end_map[[canon]] else "2020"
      dataset_id <- if (canon %in% c("par", "kdpar")) paste0(canon, "_mean_baseline_2000_2020_", depth_tag) else paste0(canon, "_baseline_2000_", end_yr, "_", depth_tag)
      time_query <- sprintf("(%s-01-01):1:(%s-01-01T00:00:00Z)", start_year, start_year)
      var_call <- if (canon %in% c("par", "kdpar")) paste0(canon, "_mean_", algorithm) else paste0(canon, "_", algorithm)
    } else {
      if (is.null(ssp)) cli::cli_abort("Argument 'ssp' required for future years.")
      dataset_id <- paste0(canon, "_ssp", ssp, "_2020_2100_", depth_tag)
      time_query <- sprintf("(%s-01-01):1:(%s-01-01T00:00:00Z)", start_year, start_year)
      var_call <- paste0(canon, "_", algorithm)
    }
    
    url <- sprintf("https://erddap.bio-oracle.org/erddap/griddap/%s.nc?%s[%s][(-90.0):1:(90.0)][(-180.0):1:(180.0)]", dataset_id, var_call, time_query)
    user_name <- code_to_user_name[[canon]]
    dest_file <- file.path(envar_grids_dir(), paste0(user_name, ".nc"))
    
    handle_file(url, dest_file, canon, user_name)
  }
  
  # --------------------------------------------------------------------
  # Return Logic
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(x, processed_stack, current_global_extent)
      } else {
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = choose_resample_method(processed_stack))
        }
        processed_stack <- c(x, processed_stack)
      }
    }
    if (set_na) {
      cli::cli_alert_info("Applying NA mask...")
      master_mask <- sum(processed_stack); processed_stack <- terra::mask(processed_stack, master_mask)
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
    attr(extracted_df, "envar_crs") <- crs; attr(extracted_df, "path") <- path
    if (!is.null(path)) write.csv(extracted_df, path)
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
