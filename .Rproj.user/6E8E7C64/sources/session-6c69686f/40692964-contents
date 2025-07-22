# R/process_layers.R
#' Process downloaded layers
#' @noRd
process_layers <- function(files, target_grid, mask, extent_type, points) {
  processed_layers <- list()
  
  for (file in files) {
    cli::cli_progress_step("Processing {.file {basename(file)}}")
    
    # Read raster
    r <- terra::rast(file)

    # 1. Crop to target extent
    r_cropped <- terra::crop(r, target_grid)
    
    # 2. Resample to target grid
    r_resampled <- terra::resample(r_cropped, target_grid, method = "bilinear")
    
    # 3. Apply mask if polygon/admin
    if (extent_type %in% c("polygon", "admin") && !is.null(mask)) {
      mask_vect <- terra::vect(mask)
      r_resampled <- terra::mask(r_resampled, mask_vect)
    }
    
    # Add to list with meaningful name
    layer_name <- extract_layer_name(basename(file))
    names(r_resampled) <- layer_name
    processed_layers[[layer_name]] <- r_resampled
  }
  
  # Create stack
  if (length(processed_layers) > 0) {
    stack <- terra::rast(processed_layers)
    
    # If points, extract values
    if (extent_type == "points" && !is.null(points)) {
      values <- exactextractr::exact_extract(stack, points)
      return(values)
    }
    
    return(stack)
  } else {
    cli::cli_abort("No layers were successfully processed")
  }
}
