# R/population.R

#' Download and process Global Population Projections (SSP)
#'
#' This function downloads, processes, and extracts variables from the
#' Global Population Projections dataset (Wang et al., 2022).
#' It provides 1-km grid population distributions from 2020 to 2100 
#' under five Shared Socioeconomic Pathways (SSPs).
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item 1 - "population" ([People]) ("pop", "inhabitants", "residents", "people", "count", "census")
#' }
#'
#' \strong{Citation:}\cr
#' Wang X, Meng X, Long Y (2022). "Projecting 1 km-grid population distributions from 2020 to 2100 globally under shared socioeconomic pathways." Scientific Data 9, 563.
#' \url{https://doi.org/10.1038/s41597-022-01675-x}
#'
#' Note: Please cite original sources of primary datasets where appropriate.
#'
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param year Numeric vector. Years to download. Available from 2020 to 2100 in 5-year intervals (e.g., c(2020, 2050)).
#' @param ssp Numeric vector. Shared Socioeconomic Pathways to download (1, 2, 3, 4, or 5).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' processed <- var_get(country = "Italy", crs = 3035) %>% 
#'   population(vars = "population", year = 2050, ssp = 2)
#'   }
#' @export
population <- function(x, vars, year = 2020, ssp = 1, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Population Projections (Wang et al., 2022).\n",
    "Citation: Wang X, Meng X, Long Y (2022). Projecting 1 km-grid population distributions from 2020 to 2100 globally under shared socioeconomic pathways. Scientific Data 9, 563.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-022-01675-x}\n"
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
    land = par_list$land
    # Track cumulative global extent
    current_global_extent <- par_list$global_extent
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
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  pop_lookup <- list(
    "population" = c("population", "pop", "inhabitants", "residents", "people", "count", "census")
  )
  
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  syn2canon <- list()
  for (canon in names(pop_lookup)) {
    for (syn in pop_lookup[[canon]]) {
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
      "Unknown Population variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # Validate Year and SSP arguments
  valid_years <- seq(2020, 2100, by = 5)
  if (any(!year %in% valid_years)) {
    cli::cli_abort("Years must be in 5-year intervals from 2020 to 2100 (e.g., 2020, 2025...).")
  }
  
  if (any(!ssp %in% 1:5)) {
    cli::cli_abort("SSP must be an integer between 1 and 5.")
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name, target_filename) {
    
    # We define the zip location based on the url hash
    zip_name <- paste0("ssp_download_", digest::digest(url, algo="md5"), ".zip")
    zip_dest <- file.path(fs::path_temp("envar/pop"), zip_name)
    fs::dir_create(fs::path_temp("envar/pop"))
    
    # 1. Download Zip if not exists
    if (!fs::file_exists(zip_dest)) {
      cli::cli_alert_info("Downloading dataset for {.val {user_name}} (this may take time)...")
      success <- download_file_figshare(url, zip_dest)
      if (!success) {
        cli::cli_alert_warning("Failed to download data from {.url {url}}.")
        return(NULL)
      }
    }
    
    # 2. Extract specific TIF
    if (!fs::file_exists(dest_file)) {
      cli::cli_alert_info("Extracting {.val {target_filename}}...")
      
      # Robust extraction: List files in zip to find the actual path
      zip_contents <- try(unzip(zip_dest, list = TRUE), silent = TRUE)
      
      if (inherits(zip_contents, "try-error")) {
        cli::cli_alert_warning("Could not list contents of zip file.")
        return(NULL)
      }
      
      # Find the file that ends with our target filename (ignoring parent folders)
      # e.g., if target is "SSP5_2100.tif", matches "folder/SSP5_2100.tif" or "SSP5_2100.tif"
      match_idx <- grep(paste0(target_filename, "$"), zip_contents$Name, ignore.case = TRUE)
      
      if (length(match_idx) == 0) {
        cli::cli_alert_warning("File {.val {target_filename}} not found inside the downloaded archive.")
        return(NULL)
      }
      
      # Use the actual path found in the zip
      actual_internal_path <- zip_contents$Name[match_idx[1]]
      
      try_unzip <- try(unzip(zip_dest, files = actual_internal_path, exdir = fs::path_temp("envar/pop")), silent = TRUE)
      
      extracted_path <- file.path(fs::path_temp("envar/pop"), actual_internal_path)
      
      if (inherits(try_unzip, "try-error") || !fs::file_exists(extracted_path)) {
        cli::cli_alert_warning("Could not extract {.val {actual_internal_path}} from archive.")
        return(NULL)
      }
      
      # Move/Rename to dest_file for consistency
      fs::file_move(extracted_path, dest_file)
      
      # Clean up the directory structure created by unzip if it was nested
      internal_dir <- dirname(extracted_path)
      if (internal_dir != fs::path_temp("envar/pop")) {
        # Try to clean up empty parent folders if extraction created them
        try(fs::dir_delete(file.path(fs::path_temp("envar/pop"), strsplit(actual_internal_path, "/")[[1]][1])), silent=TRUE)
      }
    }
    
    # 3. Standard Processing
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
         # fs::file_delete(dest_file)
        }
        return(NULL)
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
      #  fs::file_delete(dest_file)
      }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
        #  fs::file_delete(dest_file)
        }
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
      
      rm(extracted)
      gc()
      if (!is_global) {
       # fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  
  ssp_urls <- list(
    "1" = "https://figshare.com/ndownloader/files/34829160",
    "2" = "https://figshare.com/ndownloader/files/34829370",
    "3" = "https://figshare.com/ndownloader/files/45894312",
    "4" = "https://figshare.com/ndownloader/files/34829385",
    "5" = "https://figshare.com/ndownloader/files/34829391"
  )
  
  cli::cli_alert_info("Processing Global Population data...")
  
  for (s in ssp) {
    current_url <- ssp_urls[[as.character(s)]]
    
    for (y in year) {
      
      # The filename we are looking for inside the zip (ignoring folder path)
      target_filename <- paste0("SSP", s, "_", y, ".tif")
      
      # Canonical name for reference
      canon <- paste0("population_ssp", s, "_", y)
      
      # User name construction
      base_name <- code_to_user_name[["population"]]
      if (is.null(base_name)) base_name <- "population"
      user_name_combo <- paste0(base_name, "_ssp", s, "_", y)
      
      # Destination for the extracted TIF - use user_name for extr_check compatibility
      dest <- file.path(envar_grids_dir(), paste0(user_name_combo, ".tif"))
      
      handle_file(current_url, dest, canon, user_name_combo, target_filename)
    }
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
          processed_stack <- terra::resample(processed_stack, x, method = choose_resample_method(processed_stack))
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
    attr(processed_stack, "land") <- land
    
    if (set_na==TRUE){
      cli::cli_alert_info("Applying NA mask...")
      master_mask <- sum(processed_stack)
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
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) {
        crs <- prev_crs
      }
    }
    
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path
    
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
