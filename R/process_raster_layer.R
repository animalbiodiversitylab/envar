# R/process_raster_layer.R

#' Choose a resampling / reprojection method for a layer or stack
#'
#' Honours `options(envar.resample_method)`. When set to "auto" (the default),
#' categorical (factor) layers use nearest-neighbour to avoid inventing class
#' codes, while continuous layers use bilinear. If a stack mixes categorical and
#' continuous layers, nearest-neighbour is chosen to protect the class codes.
#' @noRd
choose_resample_method <- function(x) {
  user_method <- getOption("envar.resample_method", "auto")
  if (!identical(user_method, "auto")) {
    return(user_method)
  }
  is_categorical <- tryCatch(any(terra::is.factor(x)), error = function(e) FALSE)
  if (isTRUE(is_categorical)) "near" else "bilinear"
}

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

  # ------------------------------------------------------------------
  # Enforce resolution constraint
  # ------------------------------------------------------------------
  # The native resolution of the source raster must not be (meaningfully)
  # coarser than the requested target resolution, since downloading a coarse
  # source onto a finer grid only yields artificially interpolated detail.
  native_km <- tryCatch({
    rr <- terra::res(layer)
    if (isTRUE(terra::is.lonlat(layer))) {
      # latitudinal degree -> km (~constant globally, ~111.32 km per degree)
      rr[2] * 111.32
    } else {
      # projected CRS assumed to be in metres
      max(rr) / 1000
    }
  }, error = function(e) NA_real_)

  # Tolerance for the rule above. `res` is treated as a value in kilometres,
  # but the 30 arc-second base grid is only ~0.927 km, and several datasets that
  # are nominally "1 km" are in fact distributed on a 0.01-degree grid
  # (~1.11 km, e.g. the Koppen-Geiger climate zones and the IUCN habitat
  # fractions). A strict tolerance wrongly rejects those near-1-km layers, so we
  # allow the source to be up to 20% coarser than the requested resolution
  # before refusing. This still catches genuinely coarse sources (e.g. a 5 km
  # layer requested at res = 1).
  res_tolerance <- 1.2

  if (!is.na(native_km) && native_km > res * res_tolerance) {
    cli::cli_abort(c(
      "A source raster (~{round(native_km, 2)} km) is coarser than the requested resolution ({.code res = {res}}, ~{res} km).",
      "i" = "Resampling it onto a finer grid would only invent detail. Increase {.arg res} in {.fn par_set} to at least {ceiling(native_km)} to match (or exceed) the native resolution."
    ))
  }

  # ------------------------------------------------------------------
  # Choose the resampling / reprojection method
  # ------------------------------------------------------------------
  # Categorical (factor) layers must use nearest-neighbour to avoid inventing
  # class codes; continuous layers default to bilinear. Users can override the
  # behaviour with options(envar.resample_method = "near" | "bilinear").
  is_categorical <- tryCatch(any(terra::is.factor(layer)), error = function(e) FALSE)
  resample_method <- choose_resample_method(layer)

  if (is_global) {
    # Global processing: keep original extent, apply resolution and CRS
    
    # First, aggregate if needed
    if (res > 1) {
      # Categorical layers must be aggregated by majority vote (modal); averaging
      # would invent meaningless intermediate class codes.
      agg_fun <- if (isTRUE(is_categorical)) "modal" else "mean"
      cli::cli_alert_info("Aggregating by factor {res} (fun: {agg_fun})...")
      layer <- terra::aggregate(layer, fact = res, fun = agg_fun, na.rm = TRUE)
    }
    
    # Project to target CRS if different
    if (target_crs != "EPSG:4326" && !grepl("4326", target_crs)) {
      cli::cli_alert_info("Projecting to {target_crs}...")
      layer <- terra::project(layer, target_crs, method = resample_method)
    }
    
    # # If crs is not WGS84 and no other crs is specified 
    # if (target_crs == "EPSG:4326" && source_crs != "EPSG:4326") {
    #   cli::cli_alert_info("Projecting to {target_crs}...")
    #   layer <- terra::project(layer, target_crs, method = "bilinear")
    # }
    
    
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
      layer_resampled <- terra::resample(layer_cropped, grid_reproj, method = resample_method)

      # Mask with reprojected mask
      mask_vect <- terra::vect(mask_reproj)
      layer_masked <- terra::mask(layer_resampled, mask_vect)

      # Project to target CRS
      layer_final <- terra::project(layer_masked, target_crs_wkt, method = resample_method)

    } else {
      # Same CRS: straightforward crop, resample, mask
      layer_cropped <- terra::crop(layer, grid, snap = "out")
      layer_resampled <- terra::resample(layer_cropped, grid, method = resample_method)
      
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
      new_stack <- terra::resample(new_stack, existing_stack, method = choose_resample_method(new_stack))
    }
    
    combined <- c(existing_stack, new_stack)
    attr(combined, "global_extent") <- combined_extent
    attr(combined, "is_global") <- TRUE
    
    return(combined)

}