# R/freshwater.R

#' Download and process EarthEnv Freshwater Environmental Variables
#'
#' This function downloads, processes, and extracts variables from the
#' Near-global freshwater-specific environmental variables dataset.
#' These variables are available at 1 km resolution and capture upstream
#' catchment characteristics, including topography, land cover, soil, and climate.
#'
#' Available variables (working synonyms in parentheses):
#'
#' Temperature:
#'
#' 1 - "monthly_tmin_average.nc" (monthly minimum temperature average, min temp average, tmin avg, tmin)
#'
#' 2 - "monthly_tmax_average.nc" (monthly maximum temperature average, max temp average, tmax avg, tmax)
#'
#' 3 - "monthly_tmin_weighted_average.nc" (monthly minimum temperature weighted, min temp weighted, tmin weighted)
#'
#' 4 - "monthly_tmax_weighted_average.nc" (monthly maximum temperature weighted, max temp weighted, tmax weighted)
#'
#' Precipitation:
#'
#' 5 - "monthly_prec_sum.nc" (monthly upstream precipitation sum, precipitation sum, precip sum, prec)
#'
#' 6 - "monthly_prec_weighted_sum.nc" (monthly upstream precipitation weighted, precipitation weighted, precip weighted)
#'
#' Hydroclimatic:
#'
#' 7 - "hydroclim_average+sum.nc" (hydroclimatic variables average, hydroclim average, hydroclim)
#'
#' 8 - "hydroclim_weighted_average+sum.nc" (hydroclimatic variables weighted, hydroclim weighted)
#'
#' Topography:
#'
#' 9 - "elevation.nc" (upstream elevation, elevation, dem)
#'
#' 10 - "slope.nc" (upstream slope, slope)
#'
#' 11 - "flow_acc.nc" (stream length, flow accumulation, flow)
#'
#' Land cover:
#'
#' 12 - "landcover_minimum.nc" (upstream landcover minimum, landcover min)
#'
#' 13 - "landcover_maximum.nc" (upstream landcover maximum, landcover max)
#'
#' 14 - "landcover_range.nc" (upstream landcover range, landcover range)
#'
#' 15 - "landcover_average.nc" (upstream landcover average, landcover avg, landcover)
#'
#' 16 - "landcover_weighted_average.nc" (upstream landcover weighted, landcover weighted)
#'
#' Geology & soil:
#'
#' 17 - "geology_weighted_sum.nc" (upstream geology, geology weighted, geology)
#'
#' 18 - "soil_minimum.nc" (upstream soil minimum, soil min)
#'
#' 19 - "soil_maximum.nc" (upstream soil maximum, soil max)
#'
#' 20 - "soil_range.nc" (upstream soil range, soil range)
#'
#' 21 - "soil_average.nc" (upstream soil average, soil avg, soil)
#'
#' 22 - "soil_weighted_average.nc" (upstream soil weighted, soil weighted)
#'
#' Quality control:
#'
#' 23 - "quality_control.nc" (quality control, qc)
#'
#' Citation:
#'
#' Domisch, S., Amatulli, G. & Jetz, W. (2015). "Near-global freshwater-specific 
#' environmental variables for biodiversity analyses in 1 km resolution." 
#' Scientific Data, 2, 150073.
#' https://doi.org/10.1038/sdata.2015.73
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer. 
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param year Numeric. Selected year(s) for extraction. Note: Most EarthEnv freshwater variables are 
#' climatologies or static layers (1.0 version, 2015), so this argument is primarily for consistency 
#' and may not filter results for static layers.
#' @param month Numeric. Selected month(s) (1-12) for extraction. Only applicable to monthly variables 
#' (e.g., tmin, tmax, prec).
#' @param algorithm Character. The aggregation method/algorithm to use to filter specific bands.
#' For Elevation and Slope layers, this maps to the specific band order:
#' "min" (Band 1), "max" (Band 2), "range" (Band 3), "avg"/"mn" (Band 4).
#' For Flow Accumulation: "length" (Band 1), "acc" (Band 2).
#' For other variables, it attempts to match the string in the layer name.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables. If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Topography with Algorithm filtering (keeping only the average/mean band)
#' processed <- var_get(country=  "Switzerland", crs = 3035) %>% 
#'   freshwater(vars= c("elevation", "slope"), algorithm= "avg")
#'
#' # Monthly climate (January and July)
#' processed <- var_get(country= "Italy", crs=3035) %>% 
#'   freshwater(vars = "tmin", month = c(1, 7))
#'   }
#' @export

freshwater <- function(x, vars, year = NULL, month = NULL, algorithm = NULL, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using EarthEnv Freshwater Environmental Variables.\n",
    "Citation: Domisch, S., Amatulli, G. & Jetz, W. (2015). Near-global freshwater-specific environmental variables for biodiversity analyses in 1 km resolution. Scientific Data.\n",
    "DOI: {.url https://doi.org/10.1038/sdata.2015.73}\n"
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
    current_global_extent <- par_list$global_extent
  } else if (par_list$type == "point") {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    crs  <- par_list$crs
    is_global <- FALSE
    is_raster_input <- FALSE
    current_global_extent <- NULL
  } else {
    cli::cli_abort("Unsupported input type.")
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  freshwater_lookup <- list(
    "monthly_tmin_average.nc"            = c("monthly minimum temperature average", "min temp average", "tmin avg", "tmin"),
    "monthly_tmax_average.nc"            = c("monthly maximum temperature average", "max temp average", "tmax avg", "tmax"),
    "monthly_prec_sum.nc"                = c("monthly upstream precipitation sum", "precipitation sum", "precip sum", "prec"),
    "monthly_tmin_weighted_average.nc"   = c("monthly minimum temperature weighted", "min temp weighted", "tmin weighted"),
    "monthly_tmax_weighted_average.nc"   = c("monthly maximum temperature weighted", "max temp weighted", "tmax weighted"),
    "monthly_prec_weighted_sum.nc"       = c("monthly upstream precipitation weighted", "precipitation weighted", "precip weighted"),
    "hydroclim_average+sum.nc"           = c("hydroclimatic variables average", "hydroclim average", "hydroclim"),
    "hydroclim_weighted_average+sum.nc"  = c("hydroclimatic variables weighted", "hydroclim weighted"),
    "elevation.nc"                       = c("upstream elevation", "elevation", "dem"),
    "slope.nc"                           = c("upstream slope", "slope"),
    "flow_acc.nc"                        = c("stream length", "flow accumulation", "flow"),
    "landcover_minimum.nc"               = c("upstream landcover minimum", "landcover min"),
    "landcover_maximum.nc"               = c("upstream landcover maximum", "landcover max"),
    "landcover_range.nc"                 = c("upstream landcover range", "landcover range"),
    "landcover_average.nc"               = c("upstream landcover average", "landcover avg", "landcover"),
    "landcover_weighted_average.nc"      = c("upstream landcover weighted", "landcover weighted"),
    "geology_weighted_sum.nc"            = c("upstream geology", "geology weighted", "geology"),
    "soil_minimum.nc"                    = c("upstream soil minimum", "soil min"),
    "soil_maximum.nc"                    = c("upstream soil maximum", "soil max"),
    "soil_range.nc"                      = c("upstream soil range", "soil range"),
    "soil_average.nc"                    = c("upstream soil average", "soil avg", "soil"),
    "soil_weighted_average.nc"           = c("upstream soil weighted", "soil weighted"),
    "quality_control.nc"                 = c("quality control", "qc")
  )
  
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  syn2canon <- list()
  for (canon in names(freshwater_lookup)) {
    for (syn in freshwater_lookup[[canon]]) {
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
    cli::cli_abort(c(
      "Unknown Freshwater variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- fs::path_temp("envar/grids")
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
        if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      if (!is.null(algorithm)) {
        algo_clean <- tolower(algorithm)
        
        if (canon %in% c("elevation.nc", "slope.nc")) {
          band_idx <- integer(0)
          if (algo_clean %in% c("min", "minimum")) {
            band_idx <- 1
          } else if (algo_clean %in% c("max", "maximum")) {
            band_idx <- 2
          } else if (algo_clean %in% c("range", "rng")) {
            band_idx <- 3
          } else if (algo_clean %in% c("avg", "average", "mean", "mn")) {
            band_idx <- 4
          }
          if (length(band_idx) > 0) {
            layer <- layer[[band_idx]]
          }
        } else if (canon == "flow_acc.nc") {
          band_idx <- integer(0)
          if (algo_clean %in% c("len", "length")) {
            band_idx <- 1
          } else if (algo_clean %in% c("acc", "accumulation", "sum")) {
            band_idx <- 2
          }
          if (length(band_idx) > 0) {
            layer <- layer[[band_idx]]
          }
        } else {
          algo_matches <- grep(algorithm, names(layer), ignore.case = TRUE)
          if (length(algo_matches) > 0) {
            layer <- terra::subset(layer, algo_matches)
            cli::cli_alert_info("Filtered layers by algorithm pattern {.val {algorithm}}.")
          }
        }
      }
      
      if (!is.null(month) && grepl("monthly", canon, ignore.case = TRUE)) {
        month_patterns <- sprintf("_%02d", as.numeric(month))
        indices <- unique(unlist(lapply(month_patterns, function(pat) {
          grep(pat, names(layer))
        })))
        if (length(indices) > 0) {
          layer <- terra::subset(layer, indices)
          cli::cli_alert_info("Filtered layers by month(s) {.val {paste(month, collapse=', ')}}.")
        } else {
          cli::cli_alert_warning("Selected month(s) not found in layers. Keeping all bands.")
        }
      }
      
      cli::cli_alert_info("Processing layer {.val {user_name}}...")
      
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
        layer1 <- result$layer
        new_extent <- result$extent
        current_global_extent <<- new_extent
        if (!is.null(processed_stack)) {
          processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        layer1 <- result
      }
      
      if (terra::nlyr(layer1) == 1) {
        names(layer1) <- user_name
      }
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      if (!is_global) fs::file_delete(dest_file)
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) fs::file_delete(dest_file)
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      col_names <- names(extracted)
      keep_cols <- c("ID")
      val_cols <- setdiff(col_names, "ID")
      
      if (!is.null(algorithm)) {
        algo_clean <- tolower(algorithm)
        if (canon %in% c("elevation.nc", "slope.nc")) {
          target_suffix <- ""
          if (algo_clean %in% c("min", "minimum")) target_suffix <- "min"
          else if (algo_clean %in% c("max", "maximum")) target_suffix <- "max"
          else if (algo_clean %in% c("range", "rng")) target_suffix <- "range"
          else if (algo_clean %in% c("avg", "average", "mean", "mn")) target_suffix <- "avg"
          if (target_suffix != "") {
            matched <- grep(target_suffix, val_cols, ignore.case = TRUE, value = TRUE)
            if (length(matched) > 0) val_cols <- matched
          }
        } else {
          matched <- grep(algorithm, val_cols, ignore.case = TRUE, value = TRUE)
          if (length(matched) > 0) val_cols <- matched
        }
      }
      
      if (!is.null(month) && grepl("monthly", canon, ignore.case = TRUE)) {
        month_patterns <- sprintf("_%02d", as.numeric(month))
        matched <- unique(unlist(lapply(month_patterns, function(pat) {
          grep(pat, val_cols, value = TRUE)
        })))
        if (length(matched) > 0) val_cols <- intersect(val_cols, matched)
      }
      
      extracted <- extracted[, c(keep_cols, val_cols), drop = FALSE]
      
      if (ncol(extracted) == 2) {
        names(extracted)[2] <- user_name
      }
      
      if (is.null(extracted_df)) {
        extracted_df <<- extracted
      } else {
        extracted_df <<- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }
      
      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")
      
      rm(extracted)
      gc()
      if (!is_global) fs::file_delete(dest_file)
    }
  }
  
  # --------------------------------------------------------------------
  # Execution Loop
  # --------------------------------------------------------------------
  base_url <- "https://data.earthenv.org/streams"
  
  cli::cli_alert_info("Starting the download of Freshwater data...")
  
  for (canon in requested_codes) {
    filename <- canon
    url <- file.path(base_url, filename)
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    user_name <- code_to_user_name[[canon]]
    handle_file(url, dest, canon, user_name)
  }
  
  # --------------------------------------------------------------------
  # Return output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")
    
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(
          existing_stack = x,
          new_stack = processed_stack,
          current_global_extent = current_global_extent
        )
      } else {
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = "bilinear")
          
        }
        processed_stack <- c(x, processed_stack)
      }
      
    }
    
    if (is_global) {
      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
    }
    
    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    if (inherits(x, "data.frame") && !inherits(x, "sf")) {
      extracted_df <- merge(x, extracted_df[, c(1, 4:ncol(extracted_df))], by = c("ID"), all = TRUE)
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) {
        crs <- prev_crs
      }
    }
    attr(extracted_df, "envar_crs") <- crs
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}