# R/process_extent.R
#' Process extent input
#' @noRd
process_extent <- function(shape = NULL, 
                           country = NULL, 
                           continent = NULL, 
                           realm = NULL, 
                           ecoregion = NULL, 
                           biome = NULL, 
                           zooregion = NULL,
                           zoorealm = NULL,
                           mountain_region = NULL,
                           mountain_region_cmec = NULL,
                           glacier_region_19 = NULL,
                           glacier_region_20 = NULL,
                           freshwater_ecoregion = NULL,
                           marine_ecoregion = NULL,
                           marine_realm = NULL,
                           marine_province = NULL,
                           pelagic_province = NULL,
                           pelagic_biome = NULL,
                           pelagic_realm = NULL,
                           alpha_hull = FALSE,
                           buffer = 0, 
                           crs = "EPSG:4326", 
                           scale = "medium",
                           land = FALSE) {
  
  extent_info <- list(type = NULL, bbox = NULL, mask = NULL)
  
  # ---- Input validation & priority resolution ----
  input_sources <- list(
    shape = !is.null(shape),
    country = !is.null(country),
    continent = !is.null(continent),
    ecoregion = !is.null(ecoregion),
    biome = !is.null(biome),
    realm = !is.null(realm),
    zooregion = !is.null(zooregion),
    zoorealm = !is.null(zoorealm),
    mountain_region = !is.null(mountain_region),
    mountain_region_cmec = !is.null(mountain_region_cmec),
    glacier_region_19 = !is.null(glacier_region_19),
    glacier_region_20 = !is.null(glacier_region_20),
    freshwater_ecoregion = !is.null(freshwater_ecoregion),
    marine_ecoregion = !is.null(marine_ecoregion),
    marine_realm = !is.null(marine_realm),
    marine_province = !is.null(marine_province),
    pelagic_province = !is.null(pelagic_province),
    pelagic_biome = !is.null(pelagic_biome),
    pelagic_realm = !is.null(pelagic_realm)
  )
  
  active_sources <- names(Filter(identity, input_sources))
  
  if (length(active_sources) > 1) {
    cli::cli_alert_warning("You specified multiple extent sources: {paste(active_sources, collapse = ', ')}. Only the first in priority order will be used.")
  }
  
  # Helper function to get land boundary from Natural Earth
  get_land_boundary <- function(scale) {
    
    cli::cli_alert_info(paste0(
      "Land boundary from Natural Earth database.\n",
      "Website: {.url https://www.naturalearthdata.com/}\n"
    ))
    
    invisible(capture.output(suppressMessages(suppressWarnings(land_sf <- rnaturalearth::ne_download(
      scale = scale,
      type = "land",
      category = "physical",
      returnclass = "sf")))))
    
    return(land_sf)
  }
  
  # Helper function to apply land intersection to mask
  apply_land_intersection <- function(mask_sf, land_sf, crs) {
    # Ensure both are in the same CRS (use WGS84 for intersection, then transform)
    mask_wgs84 <- sf::st_transform(mask_sf, "EPSG:4326")
    land_wgs84 <- sf::st_transform(land_sf, "EPSG:4326")
    
    # Make geometries valid
    mask_wgs84 <- sf::st_make_valid(mask_wgs84)
    land_wgs84 <- sf::st_make_valid(land_wgs84)
    
    # Perform intersection
    cli::cli_alert_info("Intersecting extent with land boundary...")
    intersected <- sf::st_intersection(mask_wgs84, land_wgs84)
    
    if (nrow(intersected) == 0 || all(sf::st_is_empty(intersected))) {
      cli::cli_alert_warning("No intersection found between extent and land. Returning original extent.")
      return(sf::st_transform(mask_sf, crs))
    }
    
    # Transform to target CRS
    intersected <- sf::st_transform(intersected, crs)
    
    cli::cli_alert_success("Land intersection applied successfully.")
    return(intersected)
  }
  
  # Helper function to create alpha hull from points
  # Based on getDynamicAlphaHull from rangeBuilder package (Rabosky et al. 2016)
  # Uses initialAlpha = 2 and captures 99% of occurrence records
  create_alpha_hull <- function(points_sf) {
    
    cli::cli_alert_info(paste0(
      "Creating alpha hull"
    ))
    
    # Check if rangeBuilder package is available
    if (!requireNamespace("rangeBuilder", quietly = TRUE)) {
      cli::cli_abort("Package 'rangeBuilder' is required for alpha_hull = TRUE. Please install it with: install.packages('rangeBuilder')")
    }
    
    # Convert sf points to matrix of coordinates (in WGS84 for rangeBuilder)
    points_wgs84 <- sf::st_transform(points_sf, "EPSG:4326")
    coords <- sf::st_coordinates(points_wgs84)
    
    # Need at least 3 points to create an alpha hull
    if (nrow(coords) < 3) {
      cli::cli_abort("At least 3 points are required to create an alpha hull.")
    }
    
    # Create data frame with Longitude and Latitude columns for rangeBuilder
    points_df <- data.frame(
      Longitude = coords[, "X"],
      Latitude = coords[, "Y"]
    )
    
    cli::cli_alert_info("Creating alpha hull with initialAlpha = 2, fraction = 0.99 (99% of records)...")
    
    # Use getDynamicAlphaHull from rangeBuilder
    # Parameters:
    # - initialAlpha: starting alpha value = 2
    # - fraction: minimum fraction of points to include (0.99 = 99%)
    # - partCount: number of disjoint polygons allowed
    # - buff: buffer around points (we apply our own buffer later if needed)
    # - clipToCoast: "no" since we handle land intersection separately
    tryCatch({
      alpha_result <- rangeBuilder::getDynamicAlphaHull(
        x = points_df,
        fraction = 0.99,
        partCount = 1,
        initialAlpha = 2,
        buff = 0,
        clipToCoast = "no",
        verbose = TRUE
      )
      
      # getDynamicAlphaHull returns a list where:
      # - [[1]] contains the sfc geometry (POLYGON or MULTIPOLYGON)
      # - $alpha contains the final alpha value used (e.g., "alpha3")
      
      if (is.null(alpha_result) || length(alpha_result) == 0) {
        cli::cli_abort("getDynamicAlphaHull did not return a valid result")
      }
      
      # Extract the geometry from the first element
      alpha_geom <- alpha_result[[1]]
      
      # Get the alpha value used (for informational purposes)
      alpha_value <- alpha_result$alpha
      if (!is.null(alpha_value)) {
        cli::cli_alert_info("Final alpha value used: {alpha_value}")
      }
      
      # Handle the sfc geometry
      if (inherits(alpha_geom, "sfc")) {
        # It's already an sfc object, convert to sf
        alpha_sf <- sf::st_sf(geometry = alpha_geom)
      } else if (inherits(alpha_geom, "sfg")) {
        # Single geometry, wrap in sfc then sf
        alpha_sf <- sf::st_sf(geometry = sf::st_sfc(alpha_geom))
      } else if (inherits(alpha_geom, "SpatialPolygons") || 
                 inherits(alpha_geom, "SpatialPolygonsDataFrame")) {
        # Legacy sp object, convert to sf
        alpha_sf <- sf::st_as_sf(alpha_geom)
      } else if (inherits(alpha_geom, "sf")) {
        alpha_sf <- alpha_geom
      } else {
        cli::cli_abort("Unexpected output format from getDynamicAlphaHull: {class(alpha_geom)}")
      }
      
      # Ensure CRS is set to WGS84 (rangeBuilder works in WGS84)
      sf::st_crs(alpha_sf) <- "EPSG:4326"
      
      # Make geometry valid
      alpha_sf <- sf::st_make_valid(alpha_sf)
      
      # Union all polygons into a single geometry if multiple parts
      alpha_union <- sf::st_union(alpha_sf)
      alpha_sf <- sf::st_sf(geometry = alpha_union)
      sf::st_crs(alpha_sf) <- "EPSG:4326"
      
      cli::cli_alert_success("Alpha hull created successfully, capturing 99% of occurrence records.")
      
      return(alpha_sf)
      
    }, error = function(e) {
      cli::cli_alert_danger("Error creating alpha hull: {e$message}")
      cli::cli_abort("Failed to create alpha hull. Ensure you have sufficient points and they are not collinear.")
    })
  }
  
  # ---- 0. NOTHING SPECIFIED (Global extent) -----
  if (length(active_sources) == 0) {
    extent_info$type <- "polygon"
    extent_info$is_global <- TRUE
    
    # Default global extent in WGS84
    global_bbox <- sf::st_bbox(c(
      xmin = -180.00013888885,
      ymin = -90.00013888885,
      xmax = 179.99985967115,
      ymax = 83.99986041515
    ), crs = sf::st_crs(4326))
    
    extent_info$mask <- sf::st_sf(sf::st_as_sfc(global_bbox))
    
    # Transform to target CRS if different from WGS84
    target_crs <- sf::st_crs(crs)
    if (!is.na(target_crs) && !identical(sf::st_crs(4326), target_crs)) {
      extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    }
    
    extent_info$bbox <- sf::st_bbox(extent_info$mask)
    return(extent_info)
  }
  
  extent_info$is_global <- FALSE
  
  # ---- 1. SHAPE (highest priority) ----
  if (!is.null(shape)) {
    if (!inherits(shape, "sf") && !inherits(shape, "sfc")) {
      cli::cli_abort("The `shape` parameter must be an object of type `sf` or `sfc`.")
    }
    
    # Convert sfc to sf if needed
    if (inherits(shape, "sfc")) {
      shape <- sf::st_sf(geometry = shape)
    }
    
    # Ensure shape is in the target CRS
    shape_crs <- sf::st_crs(shape)
    target_crs_obj <- sf::st_crs(crs)
    
    if (!is.na(shape_crs) && !is.na(target_crs_obj) && !identical(shape_crs, target_crs_obj)) {
      shape <- sf::st_transform(shape, crs)
      cli::cli_alert_info("Transformed shape to target CRS: {crs}")
    } else if (is.na(shape_crs)) {
      # If shape has no CRS, assign the target CRS
      sf::st_crs(shape) <- crs
      cli::cli_alert_info("Assigned CRS {crs} to shape (was NA)")
    }
    
    # Check geometry type
    geom_types <- unique(as.character(sf::st_geometry_type(shape)))
    is_point <- all(geom_types %in% c("POINT", "MULTIPOINT"))
    
    if (is_point) {
      # Handle alpha_hull option for points
      if (alpha_hull) {
        extent_info$type <- "polygon"
        
        # Create alpha hull from points (works in WGS84 internally)
        alpha_polygon <- create_alpha_hull(shape)
        
        # Transform to target CRS
        alpha_polygon <- sf::st_transform(alpha_polygon, crs)
        
        # Apply buffer if specified (after alpha hull creation)
        if (buffer != 0) {
          cli::cli_alert_info("Applying buffer of {buffer} km to alpha hull...")
          buffer_dist <- convert_buffer_to_units(buffer, crs)
          alpha_polygon <- sf::st_buffer(alpha_polygon, dist = buffer_dist)
          alpha_polygon <- sf::st_union(alpha_polygon)
          alpha_polygon <- sf::st_sf(geometry = alpha_polygon)
          sf::st_crs(alpha_polygon) <- crs
        }
        
        extent_info$mask <- alpha_polygon
        extent_info$bbox <- sf::st_bbox(alpha_polygon)
        
        # Apply land intersection if requested
        if (land) {
          land_sf <- get_land_boundary(scale)
          extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
          extent_info$bbox <- sf::st_bbox(extent_info$mask)
        }
        
        # Return early to avoid duplicate land intersection at the end
        return(extent_info)
        
      } else if (buffer != 0) {
        # Original buffer-only behavior for points (no alpha hull)
        extent_info$type <- "polygon"
        
        # Convert buffer from km to appropriate units
        buffer_dist <- convert_buffer_to_units(buffer, crs)
        extent_buffered <- sf::st_buffer(shape, dist = buffer_dist)
        
        # Union all buffered points into single geometry
        extent_buffered <- sf::st_union(extent_buffered)
        extent_buffered <- sf::st_sf(geometry = extent_buffered)
        sf::st_crs(extent_buffered) <- crs
        
        extent_info$bbox <- sf::st_bbox(extent_buffered)
        extent_info$mask <- extent_buffered
        
        # Apply land intersection if requested (buffered points become polygons)
        if (land) {
          land_sf <- get_land_boundary(scale)
          extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
          extent_info$bbox <- sf::st_bbox(extent_info$mask)
        }
        
        # Return early to avoid duplicate land intersection at the end
        return(extent_info)
        
      } else {
        # Points without buffer or alpha_hull - return as points for extraction
        extent_info$type <- "point"
        extent_info$bbox <- sf::st_bbox(shape)
        extent_info$mask <- shape
        
        # Return early (no land intersection for raw points)
        return(extent_info)
      }
    } else {
      extent_info$type <- "polygon"
      extent_info$mask <- shape
      
      if (buffer != 0) {
        buffer_dist <- convert_buffer_to_units(buffer, crs)
        extent_buffered <- sf::st_buffer(shape, dist = buffer_dist)
        extent_info$bbox <- sf::st_bbox(extent_buffered)
        extent_info$mask <- extent_buffered
      } else {
        extent_info$bbox <- sf::st_bbox(shape)
      }
    }
    
    # Apply land intersection if requested (only for non-point geometries)
    if (land && extent_info$type != "point") {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 2. ECOREGIONS/BIOME/REALM (Dinerstein et al. 2017) ------
  if (!is.null(realm) || !is.null(biome) || !is.null(ecoregion)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading ecoregion/biome/realm shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Dinerstein E, Olson D, et al. (2017). An ecoregion-based approach to protecting half the terrestrial realm. BioScience 67(6): 534-545.\n",
      "DOI: {.url https://doi.org/10.1093/biosci/bix014}\n"
    ))
    
    tryCatch({
      url <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"
      temp_dir <- fs::path_temp("ecoregions_data")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "Ecoregions2017.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        all_files <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
        
        if (length(all_files) > 0) {
          ecoregions <- sf::read_sf(all_files[1])
        } else {
          cli::cli_alert_danger("No .shp file found in the extracted archive.")
        }
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
      if (!is.null(realm)) {
        extent_info$mask <- ecoregions[ecoregions$REALM == realm, ]
      }
      
      if (!is.null(biome)) {
        extent_info$mask <- ecoregions[ecoregions$BIOME_NAME == biome, ]
      }
      
      if (!is.null(ecoregion)) {
        extent_info$mask <- ecoregions[ecoregions$ECO_NAME == ecoregion, ]
      }
      
    }, error = function(e) {
      cli::cli_abort("Ecoregion/biome/realm not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 3. ZOOGEOGRAPHIC REGIONS/REALMS (Holt et al. 2013) ------
  if (!is.null(zooregion) || !is.null(zoorealm)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading zoogeographic region/realm shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Holt BG, Lessard JP, Borregaard MK, et al. (2013). An update of Wallace's zoogeographic regions of the world. Science 339(6115): 74-78.\n",
      "DOI: {.url https://doi.org/10.1126/science.1228282}\n"
    ))
    
    tryCatch({
      url <- "https://macroecology.ku.dk/resources/wallace/cmec_regions___realms.zip"
      temp_dir <- fs::path_temp("zooregions_data")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "cmec_regions___realms.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # The folder inside is "CMEC regions & realms"
        cmec_folder <- file.path(extract_dir, "CMEC regions & realms")
        
        if (!is.null(zoorealm)) {
          # Use newRealms shapefile for realms
          realms_shp <- file.path(cmec_folder, "newRealms.shp")
          if (!file.exists(realms_shp)) {
            # Try to find it
            realms_shp <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*newRealms.shp")
            if (length(realms_shp) == 0) {
              cli::cli_abort("newRealms.shp not found in the extracted archive.")
            }
            realms_shp <- realms_shp[1]
          }
          
          zoorealms_sf <- sf::read_sf(realms_shp)
          extent_info$mask <- zoorealms_sf[zoorealms_sf$Realm == zoorealm, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_realms <- unique(zoorealms_sf$Realm)
            cli::cli_abort("Zoorealm '{zoorealm}' not found. Available realms: {paste(available_realms, collapse = ', ')}")
          }
        }
        
        if (!is.null(zooregion)) {
          # Use Regions shapefile for regions
          regions_shp <- file.path(cmec_folder, "Regions.shp")
          if (!file.exists(regions_shp)) {
            # Try to find it
            regions_shp <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*Regions.shp")
            if (length(regions_shp) == 0) {
              cli::cli_abort("Regions.shp not found in the extracted archive.")
            }
            regions_shp <- regions_shp[1]
          }
          
          zooregions_sf <- sf::read_sf(regions_shp)
          
          # The Regions shapefile is in World_Plate_Carree projection, need to transform to WGS84 first
          # then to target CRS
          sf::st_crs(zooregions_sf) <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=12 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
          zooregions_sf <- sf::st_transform(zooregions_sf, "EPSG:4326")
          
          extent_info$mask <- zooregions_sf[zooregions_sf$Regions == zooregion, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_regions <- unique(zooregions_sf$Regions)
            cli::cli_abort("Zooregion '{zooregion}' not found. Available regions: {paste(available_regions, collapse = ', ')}")
          }
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Zoogeographic region/realm not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 4. MOUNTAIN REGIONS (GMBA Inventory v2.0) ------
  if (!is.null(mountain_region)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading GMBA Mountain Inventory shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Snethlage MA, Geschke J, Ranipeta A, et al. (2022). GMBA Mountain Inventory v2. GMBA-EarthEnv.\n",
      "DOI: {.url https://doi.org/10.48601/earthenv-t9k2-1407}\n"
    ))
    
    tryCatch({
      url <- "https://data.earthenv.org/mountains/standard/GMBA_Inventory_v2.0_standard_300.zip"
      temp_dir <- fs::path_temp("mountain_data")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "GMBA_Inventory_v2.0_standard_300.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile
        shp_file <- file.path(extract_dir, "GMBA_Inventory_v2.0_standard_300.shp")
        if (!file.exists(shp_file)) {
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
          if (length(shp_file) == 0) {
            cli::cli_abort("No .shp file found in the extracted archive.")
          }
          shp_file <- shp_file[1]
        }
        
        mountains_sf <- sf::read_sf(shp_file)
        extent_info$mask <- mountains_sf[mountains_sf$MapName == mountain_region, ]
        
        if (nrow(extent_info$mask) == 0) {
          # Try partial matching
          matches <- grep(mountain_region, mountains_sf$MapName, ignore.case = TRUE, value = TRUE)
          if (length(matches) > 0) {
            cli::cli_abort("Mountain region '{mountain_region}' not found. Did you mean one of: {paste(head(matches, 10), collapse = ', ')}?")
          } else {
            cli::cli_abort("Mountain region '{mountain_region}' not found.")
          }
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Mountain region not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 4.1 MOUNTAIN REGIONS (CMEC) ------
  if (!is.null(mountain_region_cmec)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading CMEC mountain regions shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Rahbek, C., Borregaard, M. K., Colwell, R. K., et al. (2019). Humboldt’s enigma: What causes global patterns of mountain biodiversity?. Science 365, 1108-1113.\n",
      "DOI: {.url https://doi.org/10.1126/science.aax0149}\n"
    ))
    
    tryCatch({
      url <- "https://macroecology.ku.dk/resources/mountain_regions/mountainregions2.zip"
      temp_dir <- fs::path_temp("mountain_data_cmec")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "mountainregions2.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile
        shp_file <- file.path(extract_dir, "CMEC_Mountains_Enh2018.shp")
        if (!file.exists(shp_file)) {
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
          if (length(shp_file) == 0) {
            cli::cli_abort("No .shp file found in the extracted archive.")
          }
          shp_file <- shp_file[1]
        }
        
        mountains_sf <- sf::read_sf(shp_file)
        extent_info$mask <- mountains_sf[mountains_sf$Name == mountain_region_cmec, ]
        
        if (nrow(extent_info$mask) == 0) {
          # Try partial matching
          matches <- grep(mountain_region, mountains_sf$MapName, ignore.case = TRUE, value = TRUE)
          if (length(matches) > 0) {
            cli::cli_abort("Mountain region '{mountain_region}' not found. Did you mean one of: {paste(head(matches, 10), collapse = ', ')}?")
          } else {
            cli::cli_abort("Mountain region '{mountain_region}' not found.")
          }
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Mountain region not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 5. GLACIER REGIONS 2020 (GTN-G 2023) ------
  if (!is.null(glacier_region_20)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading GTN-G glacier regions shapefile (2023)")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "GTN-G (2023). GTN-G Glacier Regions. Global Terrestrial Network for Glaciers.\n",
      "Website: {.url https://www.gtn-g.ch/}\n",
      "See also: RGI Consortium (2023). Randolph Glacier Inventory 7.0.\n",
      "DOI: {.url https://doi.org/10.5067/f6jmovy5navz}\n"
    ))
    
    tryCatch({
      url <- "https://www.gtn-g.ch/database/GlacReg_2023.zip"
      temp_dir <- fs::path_temp("glacier_data_20")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "GlacReg_2023.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile
        shp_file <- file.path(extract_dir, "GNT-G_202307_o1regions.shp")
        if (!file.exists(shp_file)) {
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
          if (length(shp_file) == 0) {
            cli::cli_abort("No .shp file found in the extracted archive.")
          }
          shp_file <- shp_file[1]
        }
        
        glaciers_sf <- sf::read_sf(shp_file)
        extent_info$mask <- glaciers_sf[glaciers_sf$o1region == glacier_region_20, ]
        
        if (nrow(extent_info$mask) == 0) {
          available_regions <- unique(glaciers_sf$o1region)
          cli::cli_abort("Glacier region '{glacier_region_20}' not found. Available regions: {paste(available_regions, collapse = ', ')}")
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Glacier region not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 6. GLACIER REGIONS 2017 (RGI v6.0) ------
  if (!is.null(glacier_region_19)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading GTN-G glacier regions shapefile (2017)")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "RGI Consortium (2017). Randolph Glacier Inventory 6.0. NSIDC.\n",
      "DOI: {.url https://doi.org/10.7265/N5-RGI-60}\n",
      "See also: Pfeffer WT, et al. (2014). The Randolph Glacier Inventory: a globally complete inventory of glaciers. J. Glaciol. 60(221): 537-552.\n",
      "DOI: {.url https://doi.org/10.3189/2014JoG13J176}\n"
    ))
    
    tryCatch({
      url <- "https://www.gtn-g.ch/database/GlacReg_2017.zip"
      temp_dir <- fs::path_temp("glacier_data_19")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "GlacReg_2017.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile
        shp_file <- file.path(extract_dir, "GNT-G_glacier_regions_201707.shp")
        if (!file.exists(shp_file)) {
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
          if (length(shp_file) == 0) {
            cli::cli_abort("No .shp file found in the extracted archive.")
          }
          shp_file <- shp_file[1]
        }
        
        glaciers_sf <- sf::read_sf(shp_file)
        extent_info$mask <- glaciers_sf[glaciers_sf$RGI_CODE == glacier_region_19, ]
        
        if (nrow(extent_info$mask) == 0) {
          available_regions <- unique(glaciers_sf$RGI_CODE)
          cli::cli_abort("Glacier region '{glacier_region_19}' not found. Available RGI codes: {paste(available_regions, collapse = ', ')}")
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Glacier region not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 7. FRESHWATER ECOREGIONS (FEOW) ------
  if (!is.null(freshwater_ecoregion)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading Freshwater Ecoregions of the World shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Abell R, Thieme ML, Revenga C, et al. (2008). Freshwater ecoregions of the world: A new map of biogeographic units for freshwater biodiversity conservation. BioScience 58(5): 403-414.\n",
      "DOI: {.url https://doi.org/10.1641/B580507}\n",
      "Website: {.url https://www.feow.org/}\n"
    ))
    
    tryCatch({
      url <- "https://www.feow.org/files/downloads/GIS_hs_snapped.zip"
      temp_dir <- fs::path_temp("freshwater_data")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "GIS_hs_snapped.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile
        shp_file <- file.path(extract_dir, "feow_hydrosheds.shp")
        if (!file.exists(shp_file)) {
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
          if (length(shp_file) == 0) {
            cli::cli_abort("No .shp file found in the extracted archive.")
          }
          shp_file <- shp_file[1]
        }
        
        freshwater_sf <- sf::read_sf(shp_file)
        
        # The CRS is NA but it's actually EPSG:4326 - set it directly
        sf::st_crs(freshwater_sf) <- "EPSG:4326"
        
        # Convert freshwater_ecoregion to numeric if it's a character
        feow_id <- as.numeric(freshwater_ecoregion)
        
        extent_info$mask <- freshwater_sf[freshwater_sf$FEOW_ID == feow_id, ]
        
        if (nrow(extent_info$mask) == 0) {
          cli::cli_abort("Freshwater ecoregion with FEOW_ID '{freshwater_ecoregion}' not found. Please check the FEOW_ID at {.url https://www.feow.org/}")
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Freshwater ecoregion not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ----- 8. MARINE AND PELAGIC ECOREGIONS (MEOW & PPOW) ------
  if (!is.null(marine_ecoregion) || !is.null(marine_realm) || !is.null(marine_province) ||
      !is.null(pelagic_province) || !is.null(pelagic_biome) || !is.null(pelagic_realm)) {
    extent_info$type <- "polygon"
    cli::cli_alert_info("Downloading Marine Ecoregions and Pelagic Provinces of the World shapefile")
    
    cli::cli_alert_info(paste0(
      "Study area is defined based on the work: \n",
      "Spalding MD, Fox HE, Allen GR, et al. (2007). Marine ecoregions of the world: A bioregionalization of coastal and shelf areas. BioScience 57(7): 573-583.\n",
      "DOI: {.url https://doi.org/10.1641/B570707}\n",
      "Spalding MD, Agostini VN, Rice J, Grant SM (2012). Pelagic provinces of the world: A biogeographic classification of the world's surface pelagic waters. Ocean & Coastal Management 60: 19-30.\n",
      "DOI: {.url https://doi.org/10.1016/j.ocecoaman.2011.12.016}\n",
      "Data source: UNEP-WCMC (2007). Marine Ecoregions of the World (MEOW) and Pelagic Provinces of the World (PPOW).\n",
      "Website: {.url https://data.unep-wcmc.org/datasets/38}\n"
    ))
    
    tryCatch({
      url <- "https://wcmc.io/WCMC_036"
      temp_dir <- fs::path_temp("marine_data")
      fs::dir_create(temp_dir)
      
      zip_path <- file.path(temp_dir, "WCMC_036.zip")
      extract_dir <- file.path(temp_dir, "extracted")
      
      download_success <- download_file(url, zip_path)
      
      if (download_success) {
        utils::unzip(zip_path, exdir = extract_dir)
        
        # Find the shapefile in the nested folder structure
        shp_file <- file.path(extract_dir, "DataPack-14_001_WCMC036_MEOW_PPOW_2007_2012_v1", 
                              "01_Data", "WCMC-036-MEOW-PPOW-2007-2012.shp")
        
        if (!file.exists(shp_file)) {
          # Try to find it recursively
          shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*WCMC-036-MEOW-PPOW*.shp")
          if (length(shp_file) == 0) {
            shp_file <- fs::dir_ls(extract_dir, recurse = TRUE, glob = "*.shp")
            if (length(shp_file) == 0) {
              cli::cli_abort("No .shp file found in the extracted archive.")
            }
          }
          shp_file <- shp_file[1]
        }
        
        marine_sf <- sf::read_sf(shp_file)
        
        # ----- MEOW (Marine Ecoregions) - TYPE == "MEOW" -----
        if (!is.null(marine_realm)) {
          meow_sf <- marine_sf[marine_sf$TYPE == "MEOW", ]
          extent_info$mask <- meow_sf[meow_sf$REALM == marine_realm, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_realms <- unique(meow_sf$REALM)
            cli::cli_abort("Marine realm '{marine_realm}' not found. Available realms: {paste(available_realms, collapse = ', ')}")
          }
        }
        
        if (!is.null(marine_province)) {
          meow_sf <- marine_sf[marine_sf$TYPE == "MEOW", ]
          extent_info$mask <- meow_sf[meow_sf$PROVINC == marine_province, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_provinces <- unique(meow_sf$PROVINC)
            cli::cli_abort("Marine province '{marine_province}' not found. Available provinces: {paste(head(available_provinces, 20), collapse = ', ')}...")
          }
        }
        
        if (!is.null(marine_ecoregion)) {
          meow_sf <- marine_sf[marine_sf$TYPE == "MEOW", ]
          extent_info$mask <- meow_sf[meow_sf$ECOREGION == marine_ecoregion, ]
          
          if (nrow(extent_info$mask) == 0) {
            # Try partial matching
            matches <- grep(marine_ecoregion, meow_sf$ECOREGION, ignore.case = TRUE, value = TRUE)
            if (length(matches) > 0) {
              cli::cli_abort("Marine ecoregion '{marine_ecoregion}' not found. Did you mean one of: {paste(head(matches, 10), collapse = ', ')}?")
            } else {
              cli::cli_abort("Marine ecoregion '{marine_ecoregion}' not found.")
            }
          }
        }
        
        # ----- PPOW (Pelagic Provinces) - TYPE == "PPOW" -----
        if (!is.null(pelagic_realm)) {
          ppow_sf <- marine_sf[marine_sf$TYPE == "PPOW", ]
          extent_info$mask <- ppow_sf[ppow_sf$REALM == pelagic_realm, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_realms <- unique(ppow_sf$REALM)
            cli::cli_abort("Pelagic realm '{pelagic_realm}' not found. Available realms: {paste(available_realms, collapse = ', ')}")
          }
        }
        
        if (!is.null(pelagic_biome)) {
          ppow_sf <- marine_sf[marine_sf$TYPE == "PPOW", ]
          extent_info$mask <- ppow_sf[ppow_sf$BIOME == pelagic_biome, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_biomes <- unique(ppow_sf$BIOME)
            cli::cli_abort("Pelagic biome '{pelagic_biome}' not found. Available biomes: {paste(available_biomes, collapse = ', ')}")
          }
        }
        
        if (!is.null(pelagic_province)) {
          ppow_sf <- marine_sf[marine_sf$TYPE == "PPOW", ]
          extent_info$mask <- ppow_sf[ppow_sf$PROVINC == pelagic_province, ]
          
          if (nrow(extent_info$mask) == 0) {
            available_provinces <- unique(ppow_sf$PROVINC)
            cli::cli_abort("Pelagic province '{pelagic_province}' not found. Available provinces: {paste(head(available_provinces, 20), collapse = ', ')}...")
          }
        }
        
      } else {
        cli::cli_abort("Process stopped due to download failure.")
      }
      
    }, error = function(e) {
      cli::cli_abort("Marine/pelagic ecoregion/realm/province/biome not found: {e$message}")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ---- 9. COUNTRY ----
  if (!is.null(country)) {
    extent_info$type <- "admin"
    cli::cli_alert_info(paste0(
      "Using the Natural Earth database for study area definition.\n",
      "Website: {.url https://www.naturalearthdata.com/}\n"
    ))
    
    tryCatch({
      extent_info$mask <- rnaturalearth::ne_countries(
        country = country,
        returnclass = "sf",
        scale = scale
      )
    }, error = function(e) {
      cli::cli_abort("Country not found: {.val {country}}.")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ---- 10. CONTINENT (lowest priority) ----
  if (!is.null(continent)) {
    extent_info$type <- "admin"
    
    cli::cli_alert_info(paste0(
      "Using the GADM (Global Administrative Areas) shapefile for Europe.\n",
      "Website: {.url https://gadm.org/data.html}\n"
    ))
    
    tryCatch({
      if (continent == "Europe" || continent == "europe") {
        extent_info$mask <- Europe
      } else {
        cli::cli_alert_info(paste0(
          "Using the Natural Earth database for study area definition.\n",
          "Website: {.url https://www.naturalearthdata.com/}\n"
        ))
        
        extent_info$mask <- rnaturalearth::ne_countries(
          continent = continent,
          returnclass = "sf",
          scale = scale
        )
      }
    }, error = function(e) {
      cli::cli_abort("Continent not found: {.val {continent}}.")
    })
    
    # Transform to target CRS
    extent_info$mask <- sf::st_transform(extent_info$mask, crs)
    
    if (buffer != 0) {
      buffer_dist <- convert_buffer_to_units(buffer, crs)
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer_dist)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
      extent_info$mask <- extent_buffered
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    # Apply land intersection if requested
    if (land) {
      land_sf <- get_land_boundary(scale)
      extent_info$mask <- apply_land_intersection(extent_info$mask, land_sf, crs)
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
}

#' Convert buffer from kilometers to appropriate units based on CRS
#' 
#' This function ensures a unified interface where users always specify buffers
#' in kilometers, and the function internally converts to the appropriate units
#' based on the coordinate reference system (degrees for geographic CRS, 
#' meters for projected CRS).
#' 
#' @param buffer_km Numeric. Buffer distance in kilometers (can be negative for inward buffers).
#' @param crs Character or numeric. The coordinate reference system.
#' @return Numeric. Buffer distance in the appropriate units for the CRS.
#' @noRd
convert_buffer_to_units <- function(buffer_km, crs) {
  crs_obj <- sf::st_crs(crs)
  
  if (is.na(crs_obj)) {
    # Default to geographic assumption if CRS is NA
    cli::cli_alert_warning("CRS is NA, assuming geographic coordinates for buffer conversion.")
    buffer_dist <- buffer_km / 111
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {round(buffer_dist, 6)} degrees (geographic CRS assumed)")
    return(buffer_dist)
  }
  
  # Determine if CRS is geographic (lat/lon) or projected (typically meters)
  is_geographic <- get_crs_type(crs_obj)
  
  if (isTRUE(is_geographic)) {
    # For geographic CRS, convert km to degrees (approximate)
    # 1 degree â‰ˆ 111 km at equator
    buffer_dist <- buffer_km / 111
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {round(buffer_dist, 6)} degrees (geographic CRS)")
  } else {
    # For projected CRS, convert km to meters
    buffer_dist <- buffer_km * 1000
    cli::cli_alert_info("Buffer: {buffer_km} km converted to {buffer_dist} meters (projected CRS)")
  }
  
  return(buffer_dist)
}

#' Determine if a CRS is geographic or projected
#' 
#' @param crs_obj An sf CRS object
#' @return Logical. TRUE if geographic, FALSE if projected.
#' @noRd
get_crs_type <- function(crs_obj) {
  # Try the direct IsGeographic property first (most reliable)
  is_geographic <- tryCatch({
    crs_obj$IsGeographic
  }, error = function(e) NULL)
  
  if (!is.null(is_geographic)) {
    return(is_geographic)
  }
  
  # Fallback: check the units in the WKT
  wkt <- crs_obj$wkt
  if (!is.null(wkt) && nchar(wkt) > 0) {
    # Check for degree units (geographic)
    if (grepl("UNIT\\[\"degree\"", wkt, ignore.case = TRUE) ||
        grepl("ANGLEUNIT\\[\"degree\"", wkt, ignore.case = TRUE)) {
      return(TRUE)
    }
    # Check for meter units (projected)
    if (grepl("UNIT\\[\"metre\"", wkt, ignore.case = TRUE) ||
        grepl("UNIT\\[\"meter\"", wkt, ignore.case = TRUE) ||
        grepl("LENGTHUNIT\\[\"metre\"", wkt, ignore.case = TRUE)) {
      return(FALSE)
    }
  }
  
  # Fallback: check proj4string patterns
  proj4 <- crs_obj$proj4string
  if (!is.null(proj4)) {
    if (grepl("\\+proj=longlat|\\+proj=latlong", proj4, ignore.case = TRUE)) {
      return(TRUE)
    }
    if (grepl("\\+units=m", proj4, ignore.case = TRUE)) {
      return(FALSE)
    }
  }
  
  # Last resort: check common patterns in the input
  input <- crs_obj$input
  if (!is.null(input)) {
    # EPSG:4326 and similar are geographic
    if (grepl("4326|4269|4267", input)) {
      return(TRUE)
    }
  }
  
  # Default assumption: if we can't determine, assume projected (meters)
  cli::cli_alert_warning("Could not determine CRS type, assuming projected (meters).")
  return(FALSE)
}