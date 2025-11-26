# R/spectre.R

#' Download and process SPECTRE environmental threat layers
#'
#' `spectre()` downloads, processes, and extracts variables from the
#' **SPECTRE – Spatially Explicit ECosysTem ThREats** dataset.  
#' Each variable corresponds to a global Cloud-Optimized GeoTIFF (COG)
#' representing a different anthropogenic or climatic threat.
#'
#' The function allows users to input either:
#' - **canonical SPECTRE filenames**, e.g. `"1_1_MINING_AREA_cog"`
#' - **human-readable names**, e.g. `"mining area"`, `"forest loss"`,
#'   `"light at night"`, `"aridity trend"`, etc.
#'
#' It automatically:
#' - downloads the selected files,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use SPECTRE data, please cite:
#'
#' **Branco, V. V., Capinha, C., Rocha, J., Correia, L. & Cardoso, P. (2024).**  
#' *SPECTRE: standardized global spatial data on terrestrial SPecies and  
#' ECosystems ThREats.* Global Ecology and Biogeography, **34(1)**, e13949.  
#' https://doi.org/10.1111/geb.13949
#'
#' _Note: Many SPECTRE variables are derived from external primary datasets.  
#' Users should consult and cite the original sources listed in the SPECTRE
#' supplementary materials._
#'
#'
#' ## Available variables
#'
#' | Human-readable name          | Canonical variable code              |
#' |------------------------------|--------------------------------------|
#' | mining area                  | 1_1_MINING_AREA_cog                  |
#' | hazard potential             | 1_2_HAZARD_POTENTIAL_cog             |
#' | human density                | 1_3_HUMAN_DENSITY_cog                |
#' | built area                   | 1_4_BUILT_AREA_cog                   |
#' | road density                 | 1_5_ROAD_DENSITY_cog                 |
#' | human footprint              | 1_6_FOOTPRINT_PERC_cog               |
#' | impacted area                | 1_7_IMPACT_AREA_cog                  |
#' | modified area                | 1_8_MODIF_AREA_cog                   |
#' | human biomes                 | 1_9_HUMAN_BIOMES_cog                 |
#' | fires                        | 1_10_FIRE_OCCUR_cog                  |
#' | crops uni                    | 1_11_CROP_PERC_UNI_cog               |
#' | crops iiasa                  | 1_12_CROP_PERC_IIASA_cog             |
#' | livestock                    | 1_13_LIVESTOCK_MASS_cog              |
#' | forest loss                  | 2_1_FOREST_LOSS_PERC_cog             |
#' | forest trend                 | 2_2_FOREST_TREND_cog                 |
#' | light at night               | 3_1_LIGHT_MCDM2_cog                  |
#' | temperature trends           | 5_1_TEMP_TRENDS_cog                  |
#' | temperature significance     | 5_2_TEMP_SIGNIF_cog                  |
#' | climate extremes             | 5_3_CLIM_EXTREME_cog                 |
#' | climate velocity             | 5_4_CLIM_VELOCITY_cog                |
#' | aridity trend                | 5_5_ARIDITY_TREND_cog                |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed SPECTRE layers.  
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

spectre <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using SPECTRE global threat layers.\n",
    "Citation: Branco, V. V., et al. (2024). SPECTRE: Standardised Global Spatial Data on Terrestrial SPecies and ECosystems ThREats. Global Ecology and Biogeography.\n",
    "DOI: {.url https://doi.org/10.1111/geb.13949}\n",
    "Note: Please cite original sources of primary datasets where appropriate."
  ))
  
  par_list <- get_par(x)
  
  if (inherits(par_list[[1]], "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res  <- par_list$res
    crs  <- par_list$crs
    is_raster_input <- TRUE
  } else {
    points <- par_list$mask
    bbox_points <- par_list$bbox
    is_raster_input <- FALSE
  }
  
  processed_stack <- NULL
  extracted_df <- NULL
  
  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  spectre_lookup <- list(
    "1_1_MINING_AREA_cog"        = c("mining area","mining_area","mining"),
    "1_2_HAZARD_POTENTIAL_cog"   = c("hazard potential","hazard"),
    "1_3_HUMAN_DENSITY_cog"      = c("human density","population","pop"),
    "1_4_BUILT_AREA_cog"         = c("built area","built"),
    "1_5_ROAD_DENSITY_cog"       = c("road density","roads","road"),
    "1_6_FOOTPRINT_PERC_cog"     = c("human footprint","footprint"),
    "1_7_IMPACT_AREA_cog"        = c("impacted area","impact area"),
    "1_8_MODIF_AREA_cog"         = c("modified area","modif area"),
    "1_9_HUMAN_BIOMES_cog"       = c("human biomes","biomes"),
    "1_10_FIRE_OCCUR_cog"        = c("fires","fire"),
    "1_11_CROP_PERC_UNI_cog"     = c("crops uni","crop uni","crop"),
    "1_12_CROP_PERC_IIASA_cog"   = c("crops iiasa","iiasa crops"),
    "1_13_LIVESTOCK_MASS_cog"    = c("livestock","livestock mass"),
    "2_1_FOREST_LOSS_PERC_cog"   = c("forest loss"),
    "2_2_FOREST_TREND_cog"       = c("forest trend"),
    "3_1_LIGHT_MCDM2_cog"        = c("light at night","night light", "light"),
    "5_1_TEMP_TRENDS_cog"        = c("temperature trends","temp trends"),
    "5_2_TEMP_SIGNIF_cog"        = c("temperature significance","temp signif"),
    "5_3_CLIM_EXTREME_cog"       = c("climate extremes"),
    "5_4_CLIM_VELOCITY_cog"      = c("climate velocity","velocity"),
    "5_5_ARIDITY_TREND_cog"      = c("aridity trend","aridity")
  )
  
  # Normalizer
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(spectre_lookup)) {
    for (syn in spectre_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes
  requested_codes <- character(0)
  unmapped <- character(0)
  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      requested_codes <- c(requested_codes, syn2canon[[key]])
    } else {
      unmapped <- c(unmapped, v)
    }
  }
  
  if (length(unmapped) > 0) {
    cli::cli_abort(c(
      "Unknown SPECTRE variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, var) {
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {var}}...")
    
    success <- download_file(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {var}} from {.url {url}}.")
      return(NULL)
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
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
        cli::cli_alert_warning("Extraction failed for {.val {basename(dest_file)}}.")
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
  # Loop through requested variables
  # --------------------------------------------------------------------
  base_url <- "https://www.nic.funet.fi/index/geodata/hy/spectre/2023"
  
  cli::cli_alert_info("Starting the download of SPECTRE data...")
  
  for (canon in requested_codes) {
    filename <- paste0(canon, ".tif")
    url <- file.path(base_url, filename)
    dest <- file.path(fs::path_temp("envar/grids"), filename)
    
    handle_file(url, dest, canon)
  }
  
  # --------------------------------------------------------------------
  # Return output
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



