# R/heterogeneity.R

#' Download and process Earthenv habitat heterogeneity layers
#'
#' `heterogeneity()` downloads, processes, and extracts variables from the
#' **Earthenv habitat heterogeneity** dataset (1-km resolution).
#' Each variable corresponds to a global Cloud-Optimized GeoTIFF (COG)
#' representing different metrics of habitat heterogeneity derived from
#' remote sensing data.
#'
#' The function allows users to input either:
#' - **Canonical variable names**, e.g. `"cv"` (Coefficient of variation), `"shannon"`, `"maximum"`
#' - **Human-readable names**, e.g. `"coefficient of variation"`, `"standard deviation"`,
#'   `"dissimilarity"`, etc.
#'
#' It automatically:
#' - downloads the selected files,
#' - crops/resamples/masks to match a user-provided `SpatRaster`, **or**
#' - extracts values when `x` is point data.
#'
#'
#' ## Citation
#' If you use Earthenv habitat heterogeneity data, please cite:
#'
#' **Tuanmu MN, Jetz W (2015).**
#' *A global, remote sensing-based characterization of terrestrial habitat heterogeneity
#' for biodiversity and ecosystem modeling.* Global Ecology and Biogeography, **24(11)**, 1329-1339.
#' https://doi.org/10.1111/geb.12365
#'
#'
#' ## Available variables (1-km resolution)
#'
#' | Human-readable name           | Canonical variable name (URL suffix)   |
#' |-------------------------------|----------------------------------------|
#' | Coefficient of variation      | cv                                     |
#' | Evenness                      | evenness                               |
#' | Range                         | range                                  |
#' | Shannon (entropy)             | shannon                                |
#' | Simpson (diversity)           | simpson                                |
#' | Standard deviation            | std                                    |
#' | Contrast                      | Contrast                               |
#' | Correlation                   | Correlation                            |
#' | Dissimilarity                 | Dissimilarity                          |
#' | Entropy (texture)             | Entropy                                |
#' | Homogeneity                   | Homogeneity                            |
#' | Maximum                       | Maximum                                |
#' | Uniformity                    | Uniformity                             |
#' | Variance                      | Variance                               |
#'
#'
#' @param x A `SpatRaster`, `SpatVector`, or `sf` object defining the area or
#'          locations for extraction.
#' @param vars Character vector of variables, supplied as canonical names or
#'          friendly names.
#' @param ... Reserved for future use.
#'
#' @return
#' - If `x` is a raster: a `SpatRaster` stack of processed heterogeneity layers.
#' - If `x` contains points: a `data.frame` of extracted values.
#'
#' @export

heterogeneity <- function(x, vars, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Earthenv Habitat Heterogeneity layers (1-km).\n",
    "Citation: Tuanmu MN, Jetz W (2015). A global, remote sensing-based characterization of terrestrial habitat heterogeneity...\n",
    "DOI: {.url https://doi.org/10.1111/geb.12365}"
  ))
  
  # NOTE: Assuming get_par, download_file, and process_points are defined elsewhere in the package.
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
  # The canonical code is the short, variable-defining part of the URL/filename.
  # --------------------------------------------------------------------
  # Full URL components (variable part -> full filename part)
  hetero_url_components <- list(
    "cv" = "cv_01_05_1km_uint16.tif",
    "evenness" = "evenness_01_05_1km_uint16.tif",
    "range" = "range_01_05_1km_uint16.tif",
    "shannon" = "shannon_01_05_1km_uint16.tif",
    "simpson" = "simpson_01_05_1km_uint16.tif",
    "std" = "std_01_05_1km_uint16.tif",
    "Contrast" = "Contrast_01_05_1km_uint32.tif",
    "Correlation" = "Correlation_01_05_1km_int16.tif",
    "Dissimilarity" = "Dissimilarity_01_05_1km_uint32.tif",
    "Entropy" = "Entropy_01_05_1km_uint16.tif",
    "Homogeneity" = "Homogeneity_01_05_1km_uint16.tif",
    "Maximum" = "Maximum_01_05_1km_uint16.tif",
    "Uniformity" = "Uniformity_01_05_1km_uint16.tif",
    "Variance" = "Variance_01_05_1km_uint32.tif"
  )
  
  # Friendly-name -> canonical code mapping (canonical code is the key in hetero_url_components)
  hetero_lookup <- list(
    "cv"            = c("coefficient of variation", "coeff of variation", "cv"),
    "evenness"      = c("evenness", "even"),
    "range"         = c("range"),
    "shannon"       = c("shannon", "shannon index", "shannon entropy"),
    "simpson"       = c("simpson", "simpson index", "simpson diversity"),
    "std"           = c("standard deviation", "std dev", "std"),
    "Contrast"      = c("contrast"),
    "Correlation"   = c("correlation", "corr"),
    "Dissimilarity" = c("dissimilarity"),
    "Entropy"       = c("entropy", "texture entropy"),
    "Homogeneity"   = c("homogeneity"),
    "Maximum"       = c("maximum", "max"),
    "Uniformity"    = c("uniformity", "uniform"),
    "Variance"      = c("variance", "var")
  )
  
  # Normalizer (same as in example)
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(hetero_lookup)) {
    for (syn in hetero_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    # Also include the canonical code itself normalized
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
      "Unknown Earthenv habitat heterogeneity variables:",
      "x" = "{.val {unmapped}}"
    ))
  }
  requested_codes <- unique(requested_codes)
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file (same structure)
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
  base_url <- "https://data.earthenv.org/habitat_heterogeneity/1km"
  
  cli::cli_alert_info("Starting the download of Earthenv habitat heterogeneity data...")
  
  for (canon in requested_codes) {
    # Get the full filename from the lookup list
    filename <- hetero_url_components[[canon]]
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