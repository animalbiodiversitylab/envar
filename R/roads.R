# R/roads.R

#' Download and process Global Road Density layers
#'
#' This function downloads, processes, and extracts road density variables.
#' Each variable corresponds to a global raster (~1 km resolution) reporting the
#' total length of roads (in metres) within each grid cell, for a single road
#' class or for a group of classes.
#'
#' @details
#' \strong{The dataset}\cr
#' Road density at 1 km grid resolution over the globe, derived from the
#' OpenStreetMap database (\url{https://www.openstreetmap.org/}) accessed through
#' the GeoFabrik (\url{https://www.geofabrik.de/}) functionalities, as of
#' 30 January 2026. Out of the original OpenStreetMap categories, roads were
#' classified into five classes:
#'
#' \itemize{
#'   \item \strong{class1} - highway (sum of the original categories "motorway"
#'     and "motorway_link")
#'   \item \strong{class2} - primary (sum of "primary", "primary_link", "trunk"
#'     and "trunk_link")
#'   \item \strong{class3} - secondary (sum of "secondary" and "secondary_link")
#'   \item \strong{class4} - tertiary (sum of "tertiary" and "tertiary_link")
#'   \item \strong{class5} - quaternary and other (sum of "residential",
#'     "living_street", "unknown" and "unclassified")
#' }
#'
#' These five classes are further grouped into three aggregated layers:
#'
#' \itemize{
#'   \item \strong{primary} - sum of classes 4 and 5
#'   \item \strong{other} - sum of classes 1, 2 and 3
#'   \item \strong{all} - sum of all five classes
#' }
#'
#' Note that the aggregated layer \code{"primary"} is a group of the minor-road
#' classes (4 and 5) and is \emph{not} the same as the single class
#' \code{"class2"} (the OpenStreetMap "primary" category), while the aggregated
#' layer \code{"other"} groups the major-road classes (1, 2 and 3). In the three
#' aggregated layers, cells with no road cover are already stored as \code{NA}
#' (rather than 0) to streamline download and analyses; the five single-class
#' layers keep their original values.
#'
#' All layers report the length in metres of roads within each ~1 km cell
#' (i.e. road density per grid cell).
#'
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \itemize{
#'   \item "class1" ("class 1", "roads 1", "road class 1", "highways", "highway", "motorway")
#'   \item "class2" ("class 2", "roads 2", "road class 2", "primary class", "trunk")
#'   \item "class3" ("class 3", "roads 3", "road class 3", "secondary roads", "secondary")
#'   \item "class4" ("class 4", "roads 4", "road class 4", "tertiary roads", "tertiary")
#'   \item "class5" ("class 5", "roads 5", "road class 5", "quaternary", "local roads", "local", "residential")
#'   \item "primary" ("primary roads", "primary group", "classes 4 and 5")
#'   \item "other" ("other roads", "other group", "classes 1 2 and 3")
#'   \item "all" ("all roads", "total", "total roads", "all classes", "combined")
#' }
#'
#' If `vars` is not specified, only "all" is downloaded.
#'
#' \strong{Data source:}\cr
#' Data are hosted in an embargoed Figshare repository and are retrieved through
#' private links. Access is provided for the use of this package while the
#' repository is under embargo; please check with the data authors before
#' redistributing the layers.
#'
#' Note: Data extent is [-180, 180, -60, 84].
#'
#' @param x The output from `par_set()` defining the area or locations for extraction,
#' the reference system, and the buffer.
#' Leave this empty and use `par_set()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' Defaults to "all" (all road classes combined).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `par_set()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `par_set()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \donttest{
#' # Example 1: Download total road density for Italy
#' processed <- par_set(country = "Italy", crs = 3035) %>%
#' roads()
#'
#' # Example 2: Download single road classes
#' processed <- par_set(country = "Italy", crs = 3035) %>%
#' roads(vars = c("highways", "class5"))
#'
#' # Example 3: Download the two aggregated groups of classes
#' processed <- par_set(country = "Italy", crs = 3035) %>%
#' roads(vars = c("primary", "other"))
#'   }
#' @export

roads <- function(x, vars = "all", ...) {

  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using Global Road Density layers.\n",
    "Data are retrieved from an embargoed Figshare repository through private links.\n",
    "Please check with the data authors before redistributing these layers.\n"
  ))

  par_list <- get_par(x)

  # Determine input type
  if (!is.null(par_list$grid) && inherits(par_list$grid, "SpatRaster")) {
    grid <- par_list$grid
    mask <- par_list$mask
    res <- par_list$res
    crs <- par_list$crs
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
    crs <- par_list$crs
    is_global <- FALSE
    is_raster_input <- FALSE
    current_global_extent <- NULL
    path = par_list$path
  } else {
    cli::cli_abort("Unsupported input type.")
  }

  processed_stack <- NULL
  extracted_df <- NULL
  fn_env <- environment()

  # --------------------------------------------------------------------
  # Friendly-name -> canonical code mapping
  # --------------------------------------------------------------------
  # Note on the two group layers: "primary" is the sum of classes 4 and 5 and
  # "other" the sum of classes 1, 2 and 3, as defined by the data authors. The
  # bare word "primary" therefore names the group, not class2 (the OpenStreetMap
  # "primary" category), which keeps the unambiguous "primary class" synonym.
  roads_lookup <- list(
    "class1" = c("class 1", "roads 1", "road 1", "road class 1", "highways", "highway", "motorway", "1"),
    "class2" = c("class 2", "roads 2", "road 2", "road class 2", "primary class", "class primary", "trunk", "2"),
    "class3" = c("class 3", "roads 3", "road 3", "road class 3", "secondary roads", "secondary", "3"),
    "class4" = c("class 4", "roads 4", "road 4", "road class 4", "tertiary roads", "tertiary", "4"),
    "class5" = c("class 5", "roads 5", "road 5", "road class 5", "quaternary", "quaternary and other",
                 "local roads", "local", "residential", "5"),
    "primary" = c("primary roads", "primary group", "primary classes", "classes 4 and 5"),
    "other"   = c("other roads", "other group", "other classes", "classes 1 2 and 3"),
    "all"    = c("all roads", "total", "total roads", "all classes", "all road classes", "combined", "tot")
  )

  # Direct URL lookup (Figshare private links, embargoed repository)
  url_lookup <- list(
    "class1" = "https://figshare.com/ndownloader/files/67129016?private_link=227b75fbee9f8030e005",
    "class2" = "https://figshare.com/ndownloader/files/67129007?private_link=227b75fbee9f8030e005",
    "class3" = "https://figshare.com/ndownloader/files/67129010?private_link=227b75fbee9f8030e005",
    "class4" = "https://figshare.com/ndownloader/files/67129013?private_link=227b75fbee9f8030e005",
    "class5" = "https://figshare.com/ndownloader/files/67129019?private_link=227b75fbee9f8030e005",
    "primary" = "https://figshare.com/ndownloader/files/67236713?private_link=227b75fbee9f8030e005",
    "other"   = "https://figshare.com/ndownloader/files/67236707?private_link=227b75fbee9f8030e005",
    "all"    = "https://figshare.com/ndownloader/files/67236710?private_link=227b75fbee9f8030e005"
  )

  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }

  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(roads_lookup)) {
    for (syn in roads_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    syn2canon[[normalize_string(canon)]] <- canon
  }

  # Default to the combined layer when nothing is requested
  if (is.null(vars) || length(vars) == 0 || all(is.na(vars))) {
    vars <- "all"
  }

  # Convert requested vars to canonical codes and keep mapping to original names
  requested_codes <- character(0)
  code_to_user_name <- list() # Maps canonical code -> user's original name
  unmapped <- character(0)

  for (v in vars) {
    key <- normalize_string(v)
    if (!is.null(syn2canon[[key]])) {
      canon <- syn2canon[[key]]
      # Only add if not already present (avoid duplicates)
      if (!(canon %in% requested_codes)) {
        requested_codes <- c(requested_codes, canon)
        # Store the user's original name for this canonical code
        code_to_user_name[[canon]] <- v
      }
    } else {
      unmapped <- c(unmapped, v)
    }
  }

  if (length(unmapped) > 0) {
    cli::cli_abort(c(
      "Unknown Roads variables:",
      "x" = "{.val {unmapped}}"
    ))
  }

  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name) {
    temp_dir <- envar_grids_dir()
    fs::dir_create(temp_dir)

    cli::cli_alert_info("Downloading {.val {basename(dest_file)}} for {.val {user_name}}...")

    success <- download_file_figshare(url, dest_file)
    if (!success) {
      cli::cli_alert_warning("Failed to download {.val {user_name}} from {.url {url}}.")
      return(NULL)
    }

    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        return(NULL)
      }

      cli::cli_alert_info("Processing layer {.val {user_name}}...")

      # Process layer using standard helper
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
        # For global processing, result is a list with layer and extent
        layer1 <- result$layer
        new_extent <- result$extent

        # Update the cumulative global extent
        fn_env$current_global_extent <- new_extent

        # If we have existing layers and extent changed, crop them
        if (!is.null(processed_stack)) {
          fn_env$processed_stack <- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        # For regional processing, result is just the layer
        layer1 <- result
      }

      # Assign user-requested name to layer
      names(layer1) <- user_name

      if (is.null(processed_stack)) {
        fn_env$processed_stack <- layer1
      } else {
        fn_env$processed_stack <- c(processed_stack, layer1)
      }

      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")

      rm(layer, layer1)
      gc()

    } else {

      cli::cli_alert_info("Extracting values from {.val {user_name}}...")

      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        return(NULL)
      }

      extracted <- data.frame(extracted)
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
        names(extracted)[ncol(extracted)] <- user_name
      }

      if (is.null(extracted_df)) {
        fn_env$extracted_df <- extracted
      } else {
        fn_env$extracted_df <- merge(extracted_df, extracted[, c(1, ncol(extracted))], by = "ID", all = TRUE)
      }

      cli::cli_alert_success("Extracted {.val {user_name}} successfully.")

      rm(extracted)
      gc()
    }
  }

  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  cli::cli_alert_info("Starting the download of Roads data...")

  for (canon in requested_codes) {
    url <- url_lookup[[canon]]

    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    dest <- file.path(envar_grids_dir(), paste0(user_name, ".tif"))

    handle_file(url, dest, canon, user_name)
  }

  # --------------------------------------------------------------------
  # Return output
  # --------------------------------------------------------------------
  if (is_raster_input) {
    if (is.null(processed_stack)) cli::cli_abort("No layers were successfully processed")

    # If x was already a SpatRaster (from previous function), combine
    if (inherits(x, "SpatRaster")) {
      if (is_global) {
        processed_stack <- combine_global_rasters(
          existing_stack = x,
          new_stack = processed_stack,
          current_global_extent = current_global_extent
        )
      } else {
        # Regional mode: resample new layers to match input raster exactly
        # This ensures perfect alignment for stacking
        if (!terra::compareGeom(x, processed_stack, stopOnError = FALSE)) {
          cli::cli_alert_info("Aligning new layers to match input raster geometry...")
          processed_stack <- terra::resample(processed_stack, x, method = choose_resample_method(processed_stack))
        }
        processed_stack <- c(x, processed_stack)
      }
    }

    # Attach global extent as attribute for downstream functions
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

        processed_stack <- terra::crop(terra::mask(processed_stack, land_sf), land_sf)
      }

      attr(processed_stack, "global_extent") <- current_global_extent
      attr(processed_stack, "is_global") <- TRUE
    }

    attr(processed_stack, "set_na") <- set_na
    attr(processed_stack, "path") <- path
    attr(processed_stack, "land") <- land

    # remove NAs if necessary
    if (set_na==TRUE){

      cli::cli_alert_info("Applying NA mask...")

      master_mask <- sum(processed_stack)
      # Apply that master mask to the whole stack
      processed_stack <- terra::mask(processed_stack, master_mask)

    }

    # write if requested
    if (!is.null(path)){
      terra::writeRaster(processed_stack, path, overwrite = TRUE)
    }

    cli::cli_alert_success("All layers processed and stacked successfully")
    return(processed_stack)
  } else {
    if (is.null(extracted_df)) cli::cli_abort("No values extracted successfully")
    # Merge with previous data if x was a data.frame
    if (inherits(x, "data.frame") && !inherits(x, "sf")) {
      extracted_df <- merge(x, extracted_df[, c(1, 4:ncol(extracted_df))], by = c("ID"), all = TRUE)
      # Preserve CRS from previous extraction
      prev_crs <- attr(x, "envar_crs")
      if (!is.null(prev_crs)) {
        crs <- prev_crs
      }
    }

    # Store the CRS as an attribute for downstream functions
    # This ensures the CRS is preserved when chaining point extractions
    attr(extracted_df, "envar_crs") <- crs
    attr(extracted_df, "path") <- path

    # Write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }

    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}
