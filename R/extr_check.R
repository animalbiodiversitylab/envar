#' Check for Environmental Extrapolation
#'
#' This function evaluates whether environmental conditions in the study area
#' fall outside the range of conditions observed at calibration points. This helps 
#' identify areas where Species Distribution Model (SDM) predictions may be unreliable
#' due to extrapolation.
#'
#' @details
#' \strong{Extrapolation types}
#'
#' Extrapolation can occur in two ways:
#' \itemize{
#'   \item \strong{Strict extrapolation}: At least one variable is outside the range found 
#'     in the calibration data.
#'   \item \strong{Combinatorial extrapolation}: Each variable is within the calibration range,
#'     but the combination of predictors is new.
#' }
#'
#' This function uses the environmental overlap mask approach from Zurell et al. (2012),
#' implemented in the `mecofun` package. It uses 1 bin per variable for strict extrapolation 
#' detection and 5 bins per variable for combinatorial extrapolation detection.
#'
#' \strong{Citations:}\cr
#' Elith J, Kearney M, Phillips S (2010). "The art of modelling range-shifting species." Methods in Ecology and Evolution 1, 330-342.
#' https://doi.org/10.1111/j.2041-210X.2010.00036.x
#'
#' Zurell D, Elith J, Schroeder B (2012). "Predicting to new environments: tools for visualizing model behaviour and impacts on mapped distributions." Diversity and Distributions 18, 628-634.
#' https://doi.org/10.1111/j.1472-4642.2012.00887.x
#'
#' @param x A `SpatRaster`, `data.frame`, or a list containing the output from
#'   previous pipeline steps (e.g., from `corr_check()`).
#' @param calib_points A `data.frame` with columns `X` and `Y` containing the 
#'   coordinates of the calibration points. These are the locations where 
#'   presence/absence or occurrence data were collected for model training.
#' @param calib_crs Character or numeric. The Coordinate Reference System of the 
#'   calibration points. Can be an EPSG code (e.g., `"EPSG:4326"`, `4326`), an ESRI 
#'   code, a PROJ4 string, or WKT. Default is `"EPSG:4326"` (WGS84).
#' @param type Character vector specifying the type(s) of extrapolation to check.
#'   Options are `"strict"`, `"combinatorial"`, or `c("strict", "combinatorial")` 
#'   (default). 
#'   \itemize{
#'     \item `"strict"`: Uses 1 bin per variable (detects univariate extrapolation).
#'     \item `"combinatorial"`: Uses 5 bins per variable (detects multivariate novelty).
#'   }
#'
#' @return A `list` containing:
#' \itemize{
#'   \item All elements from the input if it was already a list (e.g., from `corr_check()`).
#'   \item `extrapolation`: Either a `SpatRaster` with layer(s) named "strict" and/or 
#'     "combinatorial" (if input was raster-based), or a `data.frame` with additional 
#'     column(s) named "strict" and/or "combinatorial" (if input was point-based).
#'     Values of 1 indicate extrapolation (novel environments), values of 0 indicate 
#'     analog environments.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Check extrapolation after getting environmental variables
#' result <- var_get(country = "Italy") %>%
#'   esalandcover(vars = c("tree", "water")) %>%
#'   chelsa(vars = "bio1", years = "1981-2010", month = 1) %>%
#'   extr_check(calib_points = my_occurrence_data)
#'
#' # Example 2: Chain with corr_check()
#' result <- var_get(pointsdf = Apollo[1:10,]) %>%
#'   esalandcover(vars = c("ice")) %>%
#'   chelsa(vars = "bio1", years = "1981-2010", month = 1) %>%
#'   corr_check() %>%
#'   extr_check(calib_points = calibration_data, type = "strict")
#'
#' # Example 3: Check only combinatorial extrapolation
#' result <- var_get(country = "Germany") %>%
#'   worldclim(vars = c("bio1", "bio12")) %>%
#'   extr_check(calib_points = occ_points, type = "combinatorial")
#' }
#' @export

extr_check <- function(x, 
                       calib_points, 
                       calib_crs = "EPSG:4326",
                       type = c("strict", "combinatorial")) {
  
  # Validate type argument
  valid_types <- c("strict", "combinatorial")
  if (!all(type %in% valid_types)) {
    cli::cli_abort("Invalid type(s): {.val {setdiff(type, valid_types)}}. Must be 'strict' and/or 'combinatorial'.")
  }
  
  # Validate calib_points
  if (!is.data.frame(calib_points)) {
    cli::cli_abort("calib_points must be a data.frame with columns 'X' and 'Y'.")
  }
  if (!all(c("X", "Y") %in% names(calib_points))) {
    cli::cli_abort("calib_points must contain columns 'X' and 'Y'.")
  }
  
  # Normalize CRS
  calib_crs <- normalize_crs(calib_crs)
  
  # -------------------------------------------------------------------------
  # Determine input type and extract relevant data
  # -------------------------------------------------------------------------
  
  is_list_input <- FALSE
  input_list <- NULL
  study_data <- NULL
  study_raster <- NULL
  extracted_df <- NULL
  is_raster_output <- FALSE
  
  # Case 1: Input is a list (from corr_check or similar)
  if (is.list(x) && !inherits(x, "SpatRaster") && !inherits(x, "data.frame")) {
    is_list_input <- TRUE
    input_list <- x
    
    # Extract the data component
    if ("data" %in% names(x)) {
      study_data <- x$data
    } else if ("extracted_df" %in% names(x)) {
      study_data <- x$extracted_df
    } else {
      cli::cli_abort("List input must contain 'data' or 'extracted_df' element.")
    }
    
    # Determine if original data was raster or points
    if (inherits(study_data, "SpatRaster")) {
      study_raster <- study_data
      is_raster_output <- TRUE
    } else if (inherits(study_data, "data.frame")) {
      extracted_df <- study_data
      is_raster_output <- FALSE
    }
  }
  
  # Case 2: Input is a SpatRaster
  if (inherits(x, "SpatRaster")) {
    study_raster <- x
    is_raster_output <- TRUE
  }
  
  # Case 3: Input is a data.frame (point extraction results)
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    extracted_df <- x
    is_raster_output <- FALSE
  }
  
  # -------------------------------------------------------------------------
  # Get predictor variable names (exclude ID, X, Y columns)
  # -------------------------------------------------------------------------
  
  if (is_raster_output) {
    pred_vars <- names(study_raster)
  } else {
    cols_to_exclude <- c("ID", "X", "Y", "x", "y", "id")
    pred_vars <- names(extracted_df)[!tolower(names(extracted_df)) %in% tolower(cols_to_exclude)]
    
    if (length(pred_vars) < 1) {
      cli::cli_abort("No predictor variables found in the data.")
    }
  }
  
  if (length(pred_vars) < 1) {
    cli::cli_abort("At least 1 predictor variable is required for extrapolation check.")
  }
  
  cli::cli_alert_info("Checking extrapolation for {length(pred_vars)} predictor variable(s): {.val {pred_vars}}")
  
  # -------------------------------------------------------------------------
  # Extract environmental values at calibration points
  # -------------------------------------------------------------------------
  
  cli::cli_alert_info("Extracting environmental values at calibration points...")
  
  # Create sf object from calibration points
  calib_sf <- sf::st_as_sf(calib_points, coords = c("X", "Y"), crs = calib_crs)
  
  if (is_raster_output) {
    # Get study raster CRS
    study_crs <- terra::crs(study_raster)
    
    # Transform calibration points to study raster CRS
    calib_sf_transformed <- sf::st_transform(calib_sf, study_crs)
    
    # Convert to terra vect for extraction
    calib_vect <- terra::vect(calib_sf_transformed)
    
    # Extract values at calibration points
    calib_extracted <- terra::extract(study_raster, calib_vect)
    
    # Remove ID column and any rows with NA
    # Use drop = FALSE to ensure it stays a data.frame even with 1 column
    calib_env <- as.data.frame(calib_extracted[, pred_vars, drop = FALSE])
    colnames(calib_env) <- pred_vars
    calib_env <- stats::na.omit(calib_env)
    
  } else {
    # For point-based data, the calibration points should either:
    # 1. Have the same predictor columns already extracted (preferred), OR
    # 2. We extract values from cached raster files at calibration point locations
    
    # Get CRS from the extracted_df attribute
    study_crs <- attr(extracted_df, "envar_crs")
    if (is.null(study_crs)) {
      study_crs <- "EPSG:4326"
      cli::cli_alert_warning("No CRS found in data, assuming EPSG:4326.")
    }
    
    # Check if calibration points already have the predictor columns
    calib_has_predictors <- all(pred_vars %in% names(calib_points))
    
    if (calib_has_predictors) {
      # Use the predictor values directly from calib_points
      cli::cli_alert_info("Using environmental values from calibration points data.frame.")
      calib_env <- as.data.frame(calib_points[, pred_vars, drop = FALSE])
      colnames(calib_env) <- pred_vars
      calib_env <- stats::na.omit(calib_env)
      
    } else {
      # Extract values at calibration points from cached raster files
      cli::cli_alert_info("Extracting environmental values at calibration points from cached rasters...")
      
      # Transform calibration points to the study CRS
      calib_sf_transformed <- sf::st_transform(calib_sf, study_crs)
      
      # Look for cached raster files in the temp directory
      temp_raster_dir <- fs::path_temp("envar/grids")
      
      calib_env <- data.frame(row.names = seq_len(nrow(calib_points)))
      extraction_success <- TRUE
      
      for (var_name in pred_vars) {
        # Try to find the raster file - check common naming patterns
        possible_files <- c(
          file.path(temp_raster_dir, paste0(var_name, ".tif")),
          file.path(temp_raster_dir, paste0(tolower(var_name), ".tif")),
          file.path(temp_raster_dir, paste0(toupper(var_name), ".tif"))
        )
        
        raster_file <- NULL
        for (pf in possible_files) {
          if (file.exists(pf)) {
            raster_file <- pf
            break
          }
        }
        
        if (is.null(raster_file)) {
          # Try to find any .tif file that might contain this variable
          all_tifs <- list.files(temp_raster_dir, pattern = "\\.tif$", full.names = TRUE)
          cli::cli_alert_warning("Could not find cached raster for variable '{var_name}'. Available files: {paste(basename(all_tifs), collapse = ', ')}")
          extraction_success <- FALSE
          break
        }
        
        # Load raster and extract values
        tryCatch({
          r <- terra::rast(raster_file)
          
          # Transform calibration points to raster CRS
          raster_crs <- terra::crs(r)
          calib_vect <- terra::vect(sf::st_transform(calib_sf_transformed, raster_crs))
          
          # Extract values
          extracted_vals <- terra::extract(r, calib_vect)
          
          # Add to calib_env
          calib_env[[var_name]] <- extracted_vals[, 2]  # Column 1 is ID, column 2 is value
          
          cli::cli_alert_success("Extracted '{var_name}' at {nrow(calib_points)} calibration points.")
          
        }, error = function(e) {
          cli::cli_alert_warning("Failed to extract '{var_name}': {e$message}")
          extraction_success <<- FALSE
        })
      }
      
      if (!extraction_success || ncol(calib_env) < length(pred_vars)) {
        cli::cli_abort(c(
          "Failed to extract environmental values at calibration points.",
          "i" = "The cached raster files may have been deleted or are not accessible.",
          "i" = "Solutions:",
          "*" = "Pre-extract environmental values for calibration points using the same pipeline,",
          "*" = "and pass the resulting data.frame to extr_check().",
          "*" = "Example: calib_env <- var_get(pointsdf = Apollo) %>% esalandcover(vars = c('ice', 'tree'))",
          "*" = "Then: extr_check(calib_points = calib_env)"
        ))
      }
      
      # Remove rows with NA
      calib_env <- stats::na.omit(calib_env)
      cli::cli_alert_info("Successfully extracted values for {nrow(calib_env)} calibration points.")
    }
    
    if (nrow(calib_env) == 0) {
      cli::cli_abort("No valid environmental values extracted for calibration points.")
    }
  }
  
  # -------------------------------------------------------------------------
  # Prepare study area data for comparison
  # -------------------------------------------------------------------------
  
  if (is_raster_output) {
    # Convert raster to data.frame for eo_mask
    # Get coordinates as well for rebuilding raster
    study_df_full <- terra::as.data.frame(study_raster, xy = TRUE, na.rm = FALSE)
    
    # Separate coordinates from predictor values
    study_coords_df <- study_df_full[, c("x", "y")]
    # Use drop = FALSE to ensure it stays a data.frame even with 1 column
    study_env <- as.data.frame(study_df_full[, pred_vars, drop = FALSE])
    colnames(study_env) <- pred_vars
    
    # Track NA positions for reconstruction
    na_mask <- apply(study_env, 1, function(row) any(is.na(row)))
    
    # Get only complete cases for eo_mask
    study_env_complete <- as.data.frame(study_env[!na_mask, , drop = FALSE])
    colnames(study_env_complete) <- pred_vars
    
  } else {
    # For point-based data
    # Use drop = FALSE to ensure it stays a data.frame even with 1 column
    study_env <- as.data.frame(extracted_df[, pred_vars, drop = FALSE])
    colnames(study_env) <- pred_vars
    na_mask <- apply(study_env, 1, function(row) any(is.na(row)))
    study_env_complete <- as.data.frame(study_env[!na_mask, , drop = FALSE])
    colnames(study_env_complete) <- pred_vars
  }
  
  if (nrow(study_env_complete) == 0) {
    cli::cli_abort("No complete cases in study area data for extrapolation check.")
  }
  
  if (nrow(calib_env) < 5) {
    cli::cli_alert_warning("Very few calibration points ({nrow(calib_env)}). Results may be unreliable.")
  }
  
  cli::cli_alert_info("Using {nrow(calib_env)} calibration points and {nrow(study_env_complete)} study area cells/points.")
  
  # -------------------------------------------------------------------------
  # Calculate extrapolation using mecofun::eo_mask (or custom implementation for single variable)
  # -------------------------------------------------------------------------
  
  results_list <- list()
  
  # Custom eo_mask function that handles single-column data frames
  # This is needed because mecofun::eo_mask uses apply() which drops dimensions
  eo_mask_safe <- function(traindata, newdata, nbin = 5, type = "EO") {
    # Ensure inputs are data frames
    traindata <- as.data.frame(traindata)
    newdata <- as.data.frame(newdata)
    
    # Get min and max from training data
    train_minima <- sapply(traindata, min, na.rm = TRUE)
    train_maxima <- sapply(traindata, max, na.rm = TRUE)
    
    # Function to calculate bin IDs for a single row
    get_bin_id <- function(row, minima, maxima, nbin) {
      # Normalize to 0-1 range
      normalized <- (row - minima) / (maxima - minima)
      # Convert to bin number (1 to nbin)
      bins <- ceiling(normalized * nbin)
      # Handle edge cases
      bins[bins == 0] <- 1
      bins[bins < 1] <- 0
      bins[bins > nbin] <- nbin + 1
      # Handle NA from division by zero (when min == max)
      bins[is.na(bins)] <- 1
      # Create ID string
      paste(bins, collapse = ".")
    }
    
    # Calculate bin IDs for training data
    train_ids <- apply(traindata, 1, get_bin_id, 
                       minima = train_minima, 
                       maxima = train_maxima, 
                       nbin = nbin)
    train_ids_unique <- unique(train_ids)
    
    # Calculate bin IDs for new data
    new_ids <- apply(newdata, 1, get_bin_id,
                     minima = train_minima,
                     maxima = train_maxima,
                     nbin = nbin)
    
    if (type == "ID") {
      return(new_ids)
    } else if (type == "EO") {
      # Return 0 for analog (ID found in training), 1 for novel (ID not found)
      result <- ifelse(new_ids %in% train_ids_unique, 0, 1)
      return(as.numeric(result))
    }
  }
  
  if ("strict" %in% type) {
    cli::cli_alert_info("Calculating strict extrapolation (nbin = 1)...")
    
    strict_result <- eo_mask_safe(
      traindata = calib_env,
      newdata = study_env_complete,
      nbin = 1,
      type = "EO"
    )
    
    results_list$strict <- strict_result
    
    n_extrap_strict <- sum(strict_result == 1, na.rm = TRUE)
    pct_extrap_strict <- round(100 * n_extrap_strict / length(strict_result), 2)
    cli::cli_alert_info("Strict extrapolation: {n_extrap_strict} cells/points ({pct_extrap_strict}%)")
  }
  
  if ("combinatorial" %in% type) {
    cli::cli_alert_info("Calculating combinatorial extrapolation (nbin = 5)...")
    
    comb_result <- eo_mask_safe(
      traindata = calib_env,
      newdata = study_env_complete,
      nbin = 5,
      type = "EO"
    )
    
    results_list$combinatorial <- comb_result
    
    n_extrap_comb <- sum(comb_result == 1, na.rm = TRUE)
    pct_extrap_comb <- round(100 * n_extrap_comb / length(comb_result), 2)
    cli::cli_alert_info("Combinatorial extrapolation: {n_extrap_comb} cells/points ({pct_extrap_comb}%)")
  }
  
  # -------------------------------------------------------------------------
  # Construct output based on input type
  # -------------------------------------------------------------------------
  
  if (is_raster_output) {
    # Create output raster(s)
    
    # Get template from study raster (first layer)
    template_raster <- study_raster[[1]]
    
    extrap_stack <- NULL
    
    for (type_name in names(results_list)) {
      # Create a full-length vector with NAs in original positions
      full_result <- rep(NA, nrow(study_env))
      full_result[!na_mask] <- results_list[[type_name]]
      
      # Create raster from template
      result_raster <- terra::rast(template_raster)
      terra::values(result_raster) <- full_result
      names(result_raster) <- type_name
      
      if (is.null(extrap_stack)) {
        extrap_stack <- result_raster
      } else {
        extrap_stack <- c(extrap_stack, result_raster)
      }
    }
    
    extrapolation_output <- extrap_stack
    
  } else {
    # Create output data.frame
    
    # Start with original data
    result_df <- extracted_df
    
    for (type_name in names(results_list)) {
      # Create a full-length vector with NAs in original positions
      full_result <- rep(NA, nrow(extracted_df))
      full_result[!na_mask] <- results_list[[type_name]]
      
      result_df[[type_name]] <- full_result
    }
    
    extrapolation_output <- result_df
  }
  
  # -------------------------------------------------------------------------
  # Prepare final output list
  # -------------------------------------------------------------------------
  
  if (is_list_input) {
    # Add to existing list
    output <- input_list
    output$extrapolation <- extrapolation_output
  } else {
    # Create new list
    output <- list(
      data = if (is_raster_output) study_raster else extracted_df,
      extrapolation = extrapolation_output
    )
  }
  
  cli::cli_alert_success("Extrapolation check completed successfully.")
  
  return(output)
}


#' Normalize CRS to standard format (internal helper)
#' @noRd
normalize_crs <- function(crs) {
  if (is.null(crs)) {
    return("EPSG:4326")
  }
  
  crs_str <- as.character(crs)
  crs_str <- trimws(crs_str)
  
  # If it already has EPSG: or ESRI: prefix, return with standardized casing
  if (grepl("^(EPSG|ESRI):", crs_str, ignore.case = TRUE)) {
    parts <- strsplit(crs_str, ":")[[1]]
    return(paste0(toupper(parts[1]), ":", parts[2]))
  }
  
  # If it is just a number
  if (grepl("^[0-9]+$", crs_str)) {
    code_num <- as.numeric(crs_str)
    
    # Define ranges commonly reserved for ESRI authorities
    is_esri <- (code_num >= 53000 & code_num <= 54999) | (code_num >= 100000)
    
    if (is_esri) {
      return(paste0("ESRI:", crs_str))
    } else {
      return(paste0("EPSG:", crs_str))
    }
  }
  
  # Otherwise return as-is (could be PROJ4 or WKT)
  return(crs_str)
}