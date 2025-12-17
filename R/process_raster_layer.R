# R/process_raster_layer.R

#' Process a raster layer according to global or regional settings
#' @noRd
#' 
process_raster_layer <- function(layer, grid, mask, res, crs, is_global = FALSE,
                                 current_extent = NULL) {
  
  source_crs <- terra::crs(layer)
  target_crs <- crs
  
  # Check if target is geographic
  is_target_geographic <- tryCatch({
    sf::st_crs(target_crs)$IsGeographic
  }, error = function(e) {
    grepl("EPSG:4326|WGS.*84|longlat|latlong", target_crs, ignore.case = TRUE)
  })
  
  if (is_global) {
    # Global processing: keep original extent, apply resolution and CRS
    
    # First, aggregate if needed
    if (res > 1) {
      cli::cli_alert_info("Aggregating by factor {res}...")
      layer <- terra::aggregate(layer, fact = res, fun = "mean", na.rm = TRUE)
    }
    
    # Project to target CRS if different
    if (target_crs != "EPSG:4326" && !grepl("4326", target_crs)) {
      cli::cli_alert_info("Projecting to {target_crs}...")
      layer <- terra::project(layer, target_crs, method = "bilinear")
    }
    
    # Get the extent of this layer
    layer_ext <- terra::ext(layer)
    
    # Calculate intersection with current extent if one exists
    if (!is.null(current_extent)) {
      # Calculate intersection
      new_extent <- intersect_extents(current_extent, layer_ext)
      
      if (is.null(new_extent)) {
        cli::cli_abort("No overlapping extent between layers. Cannot combine datasets with non-intersecting extents.")
      }
      
      # Crop this layer to the intersection
      cli::cli_alert_info("Cropping layer to common global extent...")
      layer <- terra::crop(layer, new_extent)
      
      return(list(
        layer = layer,
        extent = new_extent
      ))
    } else {
      # First layer - just return its extent
      return(list(
        layer = layer,
        extent = layer_ext
      ))
    }
    
  } else {
    # Regional processing: crop, resample, mask
    
    # Convert mask to source CRS for cropping
    mask_crs <- sf::st_crs(mask)
    
    if (!is.na(mask_crs) && !identical(mask_crs, sf::st_crs(source_crs))) {
      mask_reproj <- sf::st_transform(mask, source_crs)
    } else {
      mask_reproj <- mask
    }
    
    # Check if source and target CRS differ
    source_crs_wkt <- terra::crs(layer)
    target_crs_wkt <- terra::crs(grid)
    crs_differ <- !identical(source_crs_wkt, target_crs_wkt)
    
    if (crs_differ) {
      # Different CRS: project grid to source, crop, then project back
      grid_reproj <- terra::project(grid, source_crs_wkt)
      
      # Crop to reprojected grid extent
      layer_cropped <- terra::crop(layer, grid_reproj, snap = "out")
      
      # Resample to reprojected grid
      layer_resampled <- terra::resample(layer_cropped, grid_reproj, method = "bilinear")
      
      # Mask with reprojected mask
      mask_vect <- terra::vect(mask_reproj)
      layer_masked <- terra::mask(layer_resampled, mask_vect)
      
      # Project to target CRS
      layer_final <- terra::project(layer_masked, target_crs_wkt, method = "bilinear")
      
    } else {
      # Same CRS: straightforward crop, resample, mask
      layer_cropped <- terra::crop(layer, grid, snap = "out")
      layer_resampled <- terra::resample(layer_cropped, grid, method = "bilinear")
      
      mask_vect <- terra::vect(mask)
      layer_final <- terra::mask(layer_resampled, mask_vect)
    }
    
    return(layer_final)
  }
}







#' Calculate the intersection of two extents
#' 
#' @param ext1 First terra extent object
#' @param ext2 Second terra extent object
#' @return A terra extent object representing the intersection, or NULL if no overlap
#' @noRd
intersect_extents <- function(ext1, ext2) {
  # Get coordinates from both extents
  xmin1 <- ext1[1]
  xmax1 <- ext1[2]
  ymin1 <- ext1[3]
  ymax1 <- ext1[4]
  
  xmin2 <- ext2[1]
  xmax2 <- ext2[2]
  ymin2 <- ext2[3]
  ymax2 <- ext2[4]
  
  # Calculate intersection
  xmin_new <- max(xmin1, xmin2)
  xmax_new <- min(xmax1, xmax2)
  ymin_new <- max(ymin1, ymin2)
  ymax_new <- min(ymax1, ymax2)
  
  # Check if there's actually an overlap
  if (xmin_new >= xmax_new || ymin_new >= ymax_new) {
    return(NULL)
  }
  
  return(terra::ext(xmin_new, xmax_new, ymin_new, ymax_new))
}


#' Align an existing raster stack to a new (smaller) extent
#' 
#' Used to crop existing layers when a new layer has a smaller extent
#' 
#' @param stack Existing SpatRaster stack
#' @param new_extent The new (smaller) extent to crop to
#' @return Cropped SpatRaster stack
#' @noRd
align_stack_to_extent <- function(stack, new_extent) {
  if (is.null(stack) || is.null(new_extent)) {
    return(stack)
  }
  
  current_ext <- terra::ext(stack)
  
  # Check if crop is actually needed
  if (isTRUE(all.equal(as.vector(current_ext), as.vector(new_extent), tolerance = 1e-6))) {
    return(stack)
  }
  
  cli::cli_alert_info("Aligning existing stack to updated common extent...")
  return(terra::crop(stack, new_extent))
}

combine_global_rasters <- function(existing_stack, new_stack, 
                                  current_global_extent = NULL) {
  
  if (is.null(existing_stack)) {
    return(new_stack)
  }
  
  if (is.null(new_stack)) {
    return(existing_stack)
  }
  
    # Global mode: align both stacks to the intersection extent
    existing_ext <- terra::ext(existing_stack)
    new_ext <- terra::ext(new_stack)
    
    combined_extent <- intersect_extents(existing_ext, new_ext)
    
    if (is.null(combined_extent)) {
      cli::cli_abort("No overlapping extent between existing layers and new layers.")
    }
    
    # Crop both to the intersection
    existing_stack <- align_stack_to_extent(existing_stack, combined_extent)
    new_stack <- align_stack_to_extent(new_stack, combined_extent)
    
    # Resample new stack to match existing if geometries still don't match
    if (!terra::compareGeom(existing_stack, new_stack, stopOnError = FALSE)) {
      cli::cli_alert_info("Resampling new layers to match existing stack...")
      new_stack <- terra::resample(new_stack, existing_stack, method = "bilinear")
    }
    
    combined <- c(existing_stack, new_stack)
    attr(combined, "global_extent") <- combined_extent
    attr(combined, "is_global") <- TRUE
    
    return(combined)

}