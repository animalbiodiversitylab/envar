# R/esalandcover.R

#' Download and process Global 1 km Land Cover variables
#'
#' `esalandcover()` downloads, processes, and extracts variables from the
#' **Global 1 km Land Cover** dataset. Each variable corresponds to a global 
#' raster representing a specific land cover class or diversity index derived 
#' from very high-resolution imagery.
#'
#' The function allows users to input either:
#' - **canonical variable codes**, e.g. `"wetland"`, `"tree"`, `"shannon"`
#' - **human-readable names**, e.g. `"forest"`, `"agriculture"`, 
#'   `"surface water"`, `"simpson index"`, etc.
#'
#' It automatically:
#' - downloads the selected files from Figshare,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use this data, please cite:
#'
#' **Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025)** #' *Global 1 km land cover for macroecological modelling from very high resolution imagery.* #' Figshare.  
#' https://figshare.com/s/4e7dee46628b530aee03
#'
#'
#' ## Available variables
#'
#' | Human-readable name          | Canonical variable code              |
#' |------------------------------|--------------------------------------|
#' | wetland / marsh              | wetland                              |
#' | bare ground / soil           | bare                                 |
#' | built / urban area           | built                                |
#' | cropland / agriculture       | cropland                             |
#' | grassland / meadow           | grass                                |
#' | ice / snow / glacier         | ice                                  |
#' | land percentage              | land_perc                            |
#' | mangrove                     | mangrove                             |
#' | moss / lichen                | moss                                 |
#' | shrub / scrub                | shrub                                |
#' | tree / forest                | tree                                 |
#' | water / surface water        | water                                |
#' | simpson index                | simpson                              |
#' | shannon index                | shannon                              |
#' | evenness index               | evenness                             |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical codes or
#'             friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed layers.  
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

esalandcover <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global 1 km Land Cover variables.\n",
    "Citation: Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025). Global 1 km land cover for macroecological modelling from very high resolution imagery. Figshare.\n",
    "DOI: {.url https://figshare.com/s/4e7dee46628b530aee03}\n"
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
  # Map user inputs to internal canonical IDs
  esa_lookup <- list(
    "wetland"   = c("wetlands", "swamp", "marsh", "bog", "fen"),
    "bare"      = c("bare ground", "bare soil", "desert", "unvegetated"),
    "built"     = c("built area", "built up", "urban", "artificial", "impervious"),
    "cropland"  = c("agriculture", "agricultural", "crop", "crops", "farming"),
    "grass"     = c("grassland", "grass land", "meadow", "pasture", "prairie"),
    "ice"       = c("snow", "snow and ice", "glacier", "ice", "permafrost"),
    "land_perc" = c("percentage of land", "land percentage", "land cover fraction", "land fraction"),
    "mangrove"  = c("mangroves", "mangrove"),
    "moss"      = c("mosses", "lichen", "moss", "lichens", "moss and lichen"),
    "shrub"     = c("shrubland", "shrub", "scrub", "bush", "thicket"),
    "tree"      = c("trees", "tree", "forest", "woodland", "canopy", "canopy cover"),
    "water"     = c("surface water", "water", "lake", "river", "freshwater"),
    "simpson"   = c("simpson index","simpson", "diversity simpson", "simpson diversity"),
    "shannon"   = c("shannon index", "entropy", "shannon", "shannon entropy", "shannon diversity"),
    "evenness"  = c("evenness index", "evenness", "pielou", "pielou evenness", "species evenness")
  )
  
  # Map canonical IDs to specific Figshare URLs
  # Note: URLs are direct download links provided in specifications
  url_lookup <- list(
    "wetland"   = "https://figshare.com/ndownloader/files/59720123?private_link=4e7dee46628b530aee03",
    "bare"      = "https://figshare.com/ndownloader/files/59720138?private_link=4e7dee46628b530aee03",
    "built"     = "https://figshare.com/ndownloader/files/59720117?private_link=4e7dee46628b530aee03",
    "cropland"  = "https://figshare.com/ndownloader/files/59720132?private_link=4e7dee46628b530aee03",
    "grass"     = "https://figshare.com/ndownloader/files/59720153?private_link=4e7dee46628b530aee03",
    "ice"       = "https://figshare.com/ndownloader/files/59720108?private_link=4e7dee46628b530aee03",
    "land_perc" = "https://figshare.com/ndownloader/files/59720111?private_link=4e7dee46628b530aee03",
    "mangrove"  = "https://figshare.com/ndownloader/files/59720099?private_link=4e7dee46628b530aee03",
    "moss"      = "https://figshare.com/ndownloader/files/59720126?private_link=4e7dee46628b530aee03",
    "shrub"     = "https://figshare.com/ndownloader/files/59720141?private_link=4e7dee46628b530aee03",
    "tree"      = "https://figshare.com/ndownloader/files/59720150?private_link=4e7dee46628b530aee03",
    "water"     = "https://figshare.com/ndownloader/files/59720129?private_link=4e7dee46628b530aee03",
    "simpson"   = "https://figshare.com/ndownloader/files/59720171?private_link=4e7dee46628b530aee03",
    "shannon"   = "https://figshare.com/ndownloader/files/59720168?private_link=4e7dee46628b530aee03",
    "evenness"  = "https://figshare.com/ndownloader/files/59720165?private_link=4e7dee46628b530aee03"
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
  for (canon in names(esa_lookup)) {
    for (syn in esa_lookup[[canon]]) {
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
      "Unknown land cover variables:",
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
  cli::cli_alert_info("Starting the download of Global Land Cover data...")
  
  for (canon in requested_codes) {
    # We assign a .tif extension for the local filename
    filename <- paste0(canon, ".tif")
    # Retrieve the specific Figshare URL from the lookup list
    url <- url_lookup[[canon]]
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