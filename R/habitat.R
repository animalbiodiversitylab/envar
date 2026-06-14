# R/habitat.R

#' Download and process IUCN Habitat Classification layers
#'
#' This function downloads, processes, and extracts variables from the
#' IUCN Global Habitat Classification Fractions dataset (Jung et al., 2020).
#' The data is available at Level 1 (broad) and Level 2 (detailed) classifications.
#'
#' @details
#' \strong{Available variables} (working synonyms in parentheses):
#'
#' \strong{Level 1 (Broad Categories)}
#' \itemize{
#'   \item "100_Forest" ([Fraction]) ("forest", "100")
#'   \item "200_Savanna" ([Fraction]) ("savanna", "200")
#'   \item "300_Shrubland" ([Fraction]) ("shrubland", "300")
#'   \item "400_Grassland" ([Fraction]) ("grassland", "400")
#'   \item "500_Wetlands inland" ([Fraction]) ("wetlands inland", "wetlands", "inland wetlands", "500")
#'   \item "600_Rocky Areas" ([Fraction]) ("rocky areas", "rocky", "600")
#'   \item "800_Desert" ([Fraction]) ("desert", "800")
#'   \item "900_Marine - Neritic" ([Fraction]) ("marine neritic", "neritic", "900")
#'   \item "1000_Marine - Oceanic" ([Fraction]) ("marine oceanic", "oceanic", "1000")
#'   \item "1100_Marine - Deep Ocean Floor" ([Fraction]) ("marine deep ocean floor", "deep ocean floor", "1100")
#'   \item "1200_Marine - Intertidal" ([Fraction]) ("marine intertidal", "intertidal", "1200")
#'   \item "1400_Artificial - Terrestrial" ([Fraction]) ("artificial terrestrial", "artificial", "terrestrial artificial", "1400")
#' }
#'
#' \strong{Level 2 (Detailed Categories - Selection)}
#' \itemize{
#'   \item "101_Forest - Boreal" ([Fraction]) ("forest boreal", "boreal forest", "101")
#'   \item "104_Forest - Temperate" ([Fraction]) ("forest temperate", "temperate forest", "104")
#'   \item "105_Forest - Subtropical-tropical dry" ([Fraction]) ("dry forest", "tropical dry forest", "105")
#'   \item "106_Forest - Subtropical-tropical moist lowland" ([Fraction]) ("moist lowland forest", "tropical moist forest", "106")
#'   \item "107_Forest - Subtropical-tropical mangrove vegetation" ([Fraction]) ("mangrove", "mangroves", "107")
#'   \item "201_Savanna - Dry" ([Fraction]) ("dry savanna", "201")
#'   \item "303_Shrubland - Boreal" ([Fraction]) ("boreal shrubland", "303")
#'   \item "308_Shrubland - Mediterranean-type" ([Fraction]) ("mediterranean shrubland", "308")
#'   \item "401_Grassland - Tundra" ([Fraction]) ("tundra", "401")
#'   \item "1401_Arable Land" ([Fraction]) ("arable land", "cropland", "1401")
#'   \item "1405_Urban Areas" ([Fraction]) ("urban areas", "urban", "city", "1405")
#'   \item (See function code for full list of Level 2 variables)
#' }
#'
#' \strong{Citation:}\cr
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V, Rondinini C, Visconti P (2020). "A global map of terrestrial habitat types." Scientific Data 7, 256.
#' https://doi.org/10.1038/s41597-020-00599-8
#' 
#' Note: Please cite original sources of primary datasets where appropriate.
#' 
#' @param x The output from `var_get()` defining the area or locations for extraction, 
#' the reference system, and the buffer.
#' Leave this empty and use `var_get()` to define parameters for download.
#' @param vars Character vector of one or more variables to download and process.
#' @param level Integer. The classification level to download. 1 (broad) or 2 (detailed). Defaults to 1.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' If `var_get()` contained a raster/polygon/points with buffer: a `SpatRaster` stack of processed variables.
#' If `var_get()` contained spatial points or data.frame of points without buffer: a `data.frame` of x, y, and extracted values.
#'
#' @examples
#' \dontrun{
#' # Example 1: Level 1 extraction (Forest and Artificial)
#' processed <- var_get(country = "Italy", crs = 3035) %>% 
#'   habitat(vars = c("Forest", "Artificial"), level = 1)
#'
#' # Example 2: Level 2 extraction (Specific biomes)
#' processed_l2 <- var_get(country = "Brazil", crs = 3035) %>% 
#'   habitat(vars = c("Mangrove", "Tropical moist lowland forest"), level = 2)
#'   }
#' @export
habitat <- function(x, vars, level = 1, ...) {
  
  # --------------------------------------------------------------------
  # Citation displayed on execution
  # --------------------------------------------------------------------
  cli::cli_alert_info(paste0(
    "Using IUCN Global Habitat Classification Fractions (Level {.val {level}}).\n",
    "Citation: Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V, Rondinini C, Visconti P (2020). A global map of terrestrial habitat types. Scientific Data 7, 256.\n",
    "DOI: {.url https://doi.org/10.1038/s41597-020-00599-8}\n"
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
    set_na=par_list$set_na
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
  # Define URLs and Filenames based on Level
  # --------------------------------------------------------------------
  
  if (level == 1) {
    base_zip_url <- "https://zenodo.org/records/4058819/files/lvl1_frac_1km_ver004.zip?download=1"
    zip_name <- "lvl1_frac_1km_ver004.zip"
    folder_prefix <- "iucn_habitatclassification_fraction_lvl1__"
    
    # Map Canonical Name (Filename part) -> Synonyms
    # Keys are the distinct middle part of the filename: Code_Name
    habitat_lookup <- list(
      "100_Forest"                 = c("forest", "100"),
      "200_Savanna"                = c("savanna", "200"),
      "300_Shrubland"              = c("shrubland", "300"),
      "400_Grassland"              = c("grassland", "400"),
      "500_Wetlands inland"        = c("wetlands inland", "wetlands", "inland wetlands", "500"),
      "600_Rocky Areas"            = c("rocky areas", "rocky", "600"),
      "800_Desert"                 = c("desert", "800"),
      "900_Marine - Neritic"       = c("marine neritic", "neritic", "900"),
      "1000_Marine - Oceanic"      = c("marine oceanic", "oceanic", "1000"),
      "1100_Marine - Deep Ocean Floor" = c("marine deep ocean floor", "deep ocean floor", "1100"),
      "1200_Marine - Intertidal"   = c("marine intertidal", "intertidal", "1200"),
      "1400_Artificial - Terrestrial" = c("artificial terrestrial", "artificial", "terrestrial artificial", "1400")
    )
    
  } else if (level == 2) {
    base_zip_url <- "https://zenodo.org/records/4058819/files/lvl2_frac_1km_ver004.zip?download=1"
    zip_name <- "lvl2_frac_1km_ver004.zip"
    folder_prefix <- "iucn_habitatclassification_fraction_lvl2__"
    
    habitat_lookup <- list(
      # Forest
      "100_Forest"                                      = c("forest", "general forest"),
      "101_Forest - Boreal"                             = c("forest boreal", "boreal forest", "101"),
      "102_Forest - Subarctic"                          = c("forest subarctic", "subarctic forest", "102"),
      "103_Forest - Subantarctic"                       = c("forest subantarctic", "subantarctic forest", "103"),
      "104_Forest - Temperate"                          = c("forest temperate", "temperate forest", "104"),
      "105_Forest - Subtropical-tropical dry"           = c("forest subtropical tropical dry", "dry forest", "tropical dry forest", "105"),
      "106_Forest - Subtropical-tropical moist lowland" = c("forest subtropical tropical moist lowland", "moist lowland forest", "tropical moist forest", "106"),
      "107_Forest - Subtropical-tropical mangrove vegetation" = c("forest mangrove", "mangrove", "mangroves", "107"),
      "108_Forest - Subtropical-tropical swamp"         = c("forest swamp", "swamp forest", "tropical swamp", "108"),
      "109_Forest - Subtropical-tropical moist montane" = c("forest moist montane", "montane forest", "cloud forest", "109"),
      
      # Savanna
      "200_Savanna"                                     = c("savanna", "general savanna"),
      "201_Savanna - Dry"                               = c("savanna dry", "dry savanna", "201"),
      "202_Savanna - Moist"                             = c("savanna moist", "moist savanna", "202"),
      
      # Shrubland
      "300_Shrubland"                                   = c("shrubland", "general shrubland"),
      "301_Shrubland - Subarctic"                       = c("shrubland subarctic", "subarctic shrubland", "301"),
      "302_Shrubland - Subantarctic"                    = c("shrubland subantarctic", "subantarctic shrubland", "302"),
      "303_Shrubland - Boreal"                          = c("shrubland boreal", "boreal shrubland", "303"),
      "304_Shrubland - Temperate"                       = c("shrubland temperate", "temperate shrubland", "304"), # Note: Hyphen spacing varies in source, using normalized logic
      "305_Shrubland - Subtropical-tropical dry"        = c("shrubland dry", "tropical dry shrubland", "305"),
      "306_Shrubland - Subtropical-tropical moist"      = c("shrubland moist", "tropical moist shrubland", "306"),
      "307_Shrubland - Subtropical-tropical high altitude" = c("shrubland high altitude", "high altitude shrubland", "307"),
      "308_Shrubland - Mediterranean-type"              = c("shrubland mediterranean", "mediterranean shrubland", "308"),
      
      # Grassland
      "400_Grassland"                                   = c("grassland", "general grassland"),
      "401_Grassland - Tundra"                          = c("grassland tundra", "tundra", "401"),
      "402_Grassland - Subarctic"                       = c("grassland subarctic", "subarctic grassland", "402"),
      "403_Grassland - Subantarctic"                    = c("grassland subantarctic", "subantarctic grassland", "403"),
      "404_Grassland - Temperate"                       = c("grassland temperate", "temperate grassland", "404"),
      "405_Grassland - Subtropical-tropical dry"        = c("grassland dry", "tropical dry grassland", "405"),
      "406_Grassland - Subtropical-tropical seasonally wet-flooded" = c("grassland seasonally wet", "wet grassland", "flooded grassland", "406"),
      "407_Grassland - Subtropical-tropical high altitude" = c("grassland high altitude", "high altitude grassland", "407"),
      
      # Wetlands
      "500_Wetlands inland"                             = c("wetlands inland", "general wetlands", "500"),
      "501_Wetlands inland - Permanent rivers-streams-creeks" = c("rivers", "streams", "creeks", "501"),
      "502_Wetlands inland - Seasonal-intermittent-irregular rivers" = c("seasonal rivers", "intermittent rivers", "502"),
      "503_Wetlands inland - Shrub dominated wetlands"  = c("shrub wetlands", "503"),
      "504_Wetlands inland - Bogs, marshes, swamps, fens, peatlands" = c("bogs", "marshes", "swamps", "fens", "peatlands", "504"),
      "505_Wetlands inland - Permanent freshwater lakes" = c("freshwater lakes", "permanent lakes", "lakes", "505"),
      "506_Wetlands inland - Seasonal-intermittent freshwater lakes" = c("seasonal lakes", "intermittent lakes", "506"),
      "507_Wetlands inland - Permanent freshwater marshes-pools" = c("freshwater marshes", "permanent marshes", "507"),
      "508_Wetlands inland - Seasonal-intermittent freshwater marshes" = c("seasonal marshes", "intermittent marshes", "508"),
      "509_Wetlands inland - Freshwater springs and oases" = c("springs", "oases", "509"),
      "510_Wetlands inland - Tundra wetlands"           = c("tundra wetlands", "510"),
      "511_Wetlands inland - Alpine wetlands"           = c("alpine wetlands", "511"),
      "512_Wetlands inland - Geothermal wetlands"       = c("geothermal wetlands", "512"),
      "513_Wetlands inland - Permanent inland deltas"   = c("inland deltas", "deltas", "513"),
      "514_Wetlands inland - Permanent saline, brackish or alkaline" = c("saline wetlands", "brackish wetlands", "514"),
      "518_Wetlands inland - Karst and other subterranean hydrological systems" = c("karst", "subterranean", "518"),
      
      # Rocky & Desert
      "600_Rocky Areas"                                 = c("rocky areas", "600"),
      "800_Desert"                                      = c("desert", "general desert", "800"),
      "801_Desert - Hot"                                = c("desert hot", "hot desert", "801"),
      "802_Desert - Temperate"                          = c("desert temperate", "temperate desert", "802"),
      "803_Desert - Cold"                               = c("desert cold", "cold desert", "803"),
      
      # Marine
      "900_Marine - Neritic"                            = c("marine neritic", "900"),
      "901_Marine - Neritic Pelagic"                    = c("marine neritic pelagic", "neritic pelagic", "901"),
      "908_Marine - Coral Reefs"                        = c("coral reefs", "reefs", "908"),
      "909_Marine - Seagrass submerged"                 = c("seagrass", "909"),
      "1000_Marine - Oceanic"                           = c("marine oceanic", "1000"),
      "1001_Marine - Epipelagic"                        = c("epipelagic", "1001"),
      "1002_Marine - Mesopelagic"                       = c("mesopelagic", "1002"),
      "1003_Marine - Bathypelagic"                      = c("bathypelagic", "1003"),
      "1004_Marine - Abyssopelagic"                     = c("abyssopelagic", "1004"),
      "1100_Marine - Deep Ocean Floor"                  = c("deep ocean floor", "1100"),
      "1101_Marine - Continental Slope-Bathyl zone"     = c("continental slope", "bathyl zone", "1101"),
      "1102_Marine - Abyssal Plain"                     = c("abyssal plain", "1102"),
      "1103_Marine - Abyssal Mountains-Hills"           = c("abyssal mountains", "1103"),
      "1104_Marine - Hadal-Deep Sea Trench"             = c("hadal", "deep sea trench", "1104"),
      "1105_Marine - Seamounts"                         = c("seamounts", "1105"),
      "1106_Marine - Deep Sea Vent"                     = c("deep sea vent", "vents", "1106"),
      "1200_Marine - Intertidal"                        = c("marine intertidal", "1200"),
      "1206_Marine - Tidepools"                         = c("tidepools", "1206"),
      "1207_Marine - Mangroves submerged Roots"         = c("mangroves submerged", "1207"),
      
      # Artificial
      "1400_Artificial - Terrestrial"                   = c("artificial terrestrial", "artificial", "1400"),
      "1401_Arable Land"                                = c("arable land", "arable", "cropland", "1401"),
      "1402_Pastureland"                                = c("pastureland", "pasture", "1402"),
      "1403_Plantations"                                = c("plantations", "1403"),
      "1404_Rural Gardens"                              = c("rural gardens", "gardens", "1404"),
      "1405_Urban Areas"                                = c("urban areas", "urban", "city", "1405")
    )
  } else {
    cli::cli_abort("Level must be 1 or 2.")
  }
  
  # Normalizer: convert to lowercase, remove punctuation, normalize whitespace
  normalize_string <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:punct:]]", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  
  # Build synonym -> canonical map
  syn2canon <- list()
  for (canon in names(habitat_lookup)) {
    for (syn in habitat_lookup[[canon]]) {
      syn2canon[[normalize_string(syn)]] <- canon
    }
    # Also map the canon name itself
    syn2canon[[normalize_string(canon)]] <- canon
  }
  
  # Convert requested vars to canonical codes AND keep mapping to original names
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
      "Unknown Habitat variables for Level {level}:",
      "x" = "{.val {unmapped}}"
    ))
  }
  
  # --------------------------------------------------------------------
  # Helper: Download, process, and clean up a single file
  # --------------------------------------------------------------------
  handle_file <- function(url, dest_file, canon, user_name, internal_file) {
    
    # Check if the specific TIF exists; if not, check/download zip and extract
    if (!file.exists(dest_file)) {
      temp_dir <- dirname(dest_file)
      zip_dest <- file.path(temp_dir, zip_name)  # Use zip_name, not basename(url) which includes ?download=1
      
      # 1. Ensure Zip exists
      if (!file.exists(zip_dest)) {
        cli::cli_alert_info("Downloading Level {.val {level}} Zip archive (approx 1GB+)...")
        success <- download_file(url, zip_dest)
        if (!success) {
          cli::cli_alert_warning("Failed to download Zip from {.url {url}}.")
          return(NULL)
        }
      }
      
      # 2. Extract specific file
      cli::cli_alert_info("Extracting {.val {user_name}} from archive...")
      
      # We extract with junkpaths=TRUE to flatten structure to temp_dir
      extract_result <- try(utils::unzip(zip_dest, files = internal_file, exdir = temp_dir, junkpaths = TRUE), silent = TRUE)
      
      # If extraction failed, maybe the internal path didn't match exactly?
      # For now assume internal_file is correct based on folder structure
      if (inherits(extract_result, "try-error") || length(extract_result) == 0) {
        # Fallback: list files in zip to find match if exact name fails? 
        # But for now strict mapping is safer.
        cli::cli_alert_warning("Could not extract {.val {internal_file}} from zip.")
        return(NULL)
      }
      
      # Rename extracted file to dest_file
      # junkpaths=TRUE extracts with original filename, need to rename to user_name.tif
      extracted_filename <- basename(internal_file)
      extracted_path <- file.path(temp_dir, extracted_filename)
      if (file.exists(extracted_path) && extracted_path != dest_file) {
        file.rename(extracted_path, dest_file)
      }
    }
    
    if (is_raster_input) {
      layer <- try(terra::rast(dest_file), silent = TRUE)
      if (inherits(layer, "try-error")) {
        cli::cli_alert_warning("Could not read raster {.val {dest_file}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
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
        current_global_extent <<- new_extent
        
        # If we have existing layers and extent changed, crop them
        if (!is.null(processed_stack)) {
          processed_stack <<- align_stack_to_extent(processed_stack, new_extent)
        }
      } else {
        # For regional processing, result is just the layer
        layer1 <- result
      }
      
      # Assign user-requested name to layer
      names(layer1) <- user_name
      
      if (is.null(processed_stack)) {
        processed_stack <<- layer1
      } else {
        processed_stack <<- c(processed_stack, layer1)
      }
      
      cli::cli_alert_success("Processed and added {.val {user_name}} to stack.")
      
      rm(layer, layer1)
      gc()
      # Clean up extracted TIF to save space, but keep Zip? 
      # Usually keeping extracted file is fine in temp.
      if (!is_global) {
        #fs::file_delete(dest_file)
      }
      
    } else {
      
      cli::cli_alert_info("Extracting values from {.val {user_name}}...")
      
      extracted <- try(process_points(file = dest_file, points = points), silent = TRUE)
      
      if (inherits(extracted, "try-error")) {
        cli::cli_alert_warning("Extraction failed for {.val {user_name}}.")
        if (!is_global) {
          #fs::file_delete(dest_file)
        }
        return(NULL)
      }
      
      extracted <- data.frame(extracted)
      
      if (ncol(extracted) >= 2) {
        # Use user-requested name for the column
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
        #fs::file_delete(dest_file)
      }
    }
  }
  
  # --------------------------------------------------------------------
  # Loop through requested variables
  # --------------------------------------------------------------------
  
  # Create temp dir for this batch
  temp_dir <- fs::path_temp("envar/habitat")
  fs::dir_create(temp_dir)
  
  cli::cli_alert_info("Processing IUCN Habitat data...")
  
  for (canon in requested_codes) {
    # Construct the exact filename expected inside the Zip
    # Structure: prefix + canon + "__ver004.tif"
    # Note: canon (e.g., "100_Forest") comes from the keys of the lookup list
    full_filename_base <- paste0(folder_prefix, canon, "__ver004.tif")
    
    # The file is inside a folder in the zip, usually named same as zip stem
    # lvl1_frac_1km_ver004/iucn_habitatclassification_fraction_lvl1__100_Forest__ver004.tif
    zip_stem <- tools::file_path_sans_ext(zip_name)
    internal_file <- file.path(zip_stem, full_filename_base)
    
    # Get the user's original name for this canonical code
    user_name <- code_to_user_name[[canon]]
    
    # Destination for extracted file - use user_name for extr_check compatibility
    grids_dir <- envar_grids_dir()
    fs::dir_create(grids_dir)
    dest <- file.path(grids_dir, paste0(user_name, ".tif"))
    
    # Pass the ZIP url, but dest is the TIF
    handle_file(base_zip_url, dest, canon, user_name, internal_file)
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
        
        processed_stack <-terra::crop(terra::mask(processed_stack, land_sf), land_sf)
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
    
    # write if requested
    if (!is.null(path)){
      write.csv(extracted_df, path)
    }
    cli::cli_alert_success("Extraction completed successfully")
    return(extracted_df)
  }
}