# R/var_get_chelsa.R
#' Download CHELSA data
#' @noRd
var_get_chelsa <- function(variables, gcm = NULL, ssp = NULL, time_period = NULL, 
                           month = NULL, year = NULL, ...) {
  base_url <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL"
  downloaded_files <- character()
  
  # Determina se stiamo scaricando dati futuri o storici
  is_future <- !is.null(gcm) && !is.null(ssp) && !is.null(time_period)
  
  for (var in variables) {
    # Gestione variabili bioclimatiche
    if (var == "bioclim") {
      if (is_future) {
        # Dati futuri - bioclim variables
        for (i in 1:19) {
          bio_num <- sprintf("%02d", i)
          # Formato: CHELSA_bio01_2011-2040_gfdl-esm4_ssp126_V.2.1.tif
          url <- sprintf("%s/climatologies/%s/%s/%s/bio/CHELSA_bio%s_%s_%s_%s_V.2.1.tif",
                         base_url, time_period, toupper(gcm), ssp, 
                         bio_num, time_period, tolower(gcm), ssp)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      } else {
        # Dati storici - bioclim variables (1981-2010)
        for (i in 1:19) {
          url <- sprintf("%s/climatologies/1981-2010/bio/CHELSA_bio%d_1981-2010_V.2.1.tif", 
                         base_url, i)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      }
    } 
    # Gestione variabili mensili (tas, tasmin, tasmax, pr)
    else if (var %in% c("tas", "tasmin", "tasmax", "pr")) {
      if (!is.null(year) && !is.null(month)) {
        # Dati mensili per anno e mese specifici
        month_str <- sprintf("%02d", month)
        url <- sprintf("%s/monthly/%s/CHELSA_%s_%s_%d_V.2.1.tif",
                       base_url, var, var, month_str, year)
        dest_file <- file.path(temp_dir, basename(url))
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      } else if (!is.null(year)) {
        # Tutti i mesi per un anno specifico
        for (m in 1:12) {
          month_str <- sprintf("%02d", m)
          url <- sprintf("%s/monthly/%s/CHELSA_%s_%s_%d_V.2.1.tif",
                         base_url, var, var, month_str, year)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      } else if (is_future) {
        # Dati futuri mensili
        for (month in 1:12) {
          month_str <- sprintf("%02d", month)
          url <- sprintf("%s/climatologies/%s/%s/%s/%s/CHELSA_%s_r1i1p1f1_w5e5_%s_%s_%s_%s_norm.tif",
                         base_url, time_period, toupper(gcm), ssp, var,
                         tolower(gcm), ssp, var, month_str,
                         gsub("-", "_", time_period))
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      } else {
        # Dati storici mensili (1981-2010)
        for (month in 1:12) {
          month_str <- sprintf("%02d", month)
          url <- sprintf("%s/climatologies/1981-2010/%s/CHELSA_%s_%s_1981-2010_V.2.1.tif",
                         base_url, var, var, month_str)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      }
    }
    # Gestione altre variabili bioclimatiche/derivate
    else if (var %in% c("ai", "clt_max", "clt_mean", "clt_min", "clt_range",
                        "cmi_max", "cmi_mean", "cmi_min", "cmi_range",
                        "fcf", "fgd", "gdd0", "gdd10", "gdd5",
                        "gddlgd0", "gddlgd10", "gddlgd5",
                        "gddfgd0", "gddfgd10", "gddfgd5",
                        "gls", "gsp", "gst",
                        "hurs_max", "hurs_mean", "hurs_min", "hurs_range",
                        "kg0", "kg1", "kg2", "kg3", "kg4", "kg5",
                        "lgd", "ngd0", "ngd10", "ngd5", "npp",
                        "pet_penman_max", "pet_penman_mean", "pet_penman_min", "pet_penman_range",
                        "rsds_max", "rsds_mean", "rsds_min", "rsds_range",
                        "scd", "sfcWind_max", "sfcWind_mean", "sfcWind_min", "sfcWind_range",
                        "swb", "swe", "vpd_max", "vpd_mean", "vpd_min", "vpd_range")) {
      
      if (is_future) {
        # Determina il percorso corretto per la variabile
        var_path <- var
        if (grepl("_", var)) {
          # Variabili con suffisso (es. clt_max)
          parts <- strsplit(var, "_")[[1]]
          var_type <- parts[1]
          var_suffix <- parts[2]
          url <- sprintf("%s/climatologies/%s/%s/%s/bio/%s/CHELSA_%s_%s_%s_%s_%s_V.2.1.tif",
                         base_url, time_period, toupper(gcm), ssp, var_type,
                         var_suffix, var_type, var_suffix, time_period, tolower(gcm), ssp)
        } else {
          # Variabili semplici
          url <- sprintf("%s/climatologies/%s/%s/%s/bio/CHELSA_%s_%s_%s_%s_V.2.1.tif",
                         base_url, time_period, toupper(gcm), ssp,
                         var, time_period, tolower(gcm), ssp)
        }
        
        dest_file <- file.path(temp_dir, basename(url))
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      } else {
        # Dati storici
        var_file <- var
        if (grepl("_", var)) {
          parts <- strsplit(var, "_")[[1]]
          var_type <- parts[1]
          var_suffix <- parts[2]
          url <- sprintf("%s/climatologies/1981-2010/bio/%s/%s/CHELSA_%s_%s_1981-2010_V.2.1.tif",
                         base_url, var_type, var_suffix, var_type, var_suffix)
        } else {
          url <- sprintf("%s/climatologies/1981-2010/bio/CHELSA_%s_1981-2010_V.2.1.tif",
                         base_url, var)
        }
        
        dest_file <- file.path(temp_dir, basename(url))
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      }
    }
    # Gestione variabili mensili climatologiche (clt, cmi, hurs, pet, rsds, sfcWind, vpd)
    else if (var %in% c("clt", "cmi", "hurs", "pet_penman", "rsds", "sfcWind", "vpd")) {
      var_folder <- ifelse(var == "pet_penman", "pet", var)
      
      if (!is.null(year) && !is.null(month)) {
        # Dati mensili per anno e mese specifici (1979-2019)
        month_str <- sprintf("%02d", month)
        url <- sprintf("%s/monthly/%s/CHELSA_%s_%s_%d_V.2.1.tif",
                       base_url, var_folder, var_folder, month_str, year)
        dest_file <- file.path(temp_dir, basename(url))
        if (download_file(url, dest_file)) {
          downloaded_files <- c(downloaded_files, dest_file)
        }
      } else if (!is.null(year)) {
        # Tutti i mesi per un anno specifico
        for (m in 1:12) {
          month_str <- sprintf("%02d", m)
          url <- sprintf("%s/monthly/%s/CHELSA_%s_%s_%d_V.2.1.tif",
                         base_url, var_folder, var_folder, month_str, year)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      } else if (is_future) {
        # I dati mensili futuri seguono la stessa struttura di tas/pr
        for (month in 1:12) {
          month_str <- sprintf("%02d", month)
          var_name <- ifelse(var == "pet_penman", "pet", var)
          url <- sprintf("%s/climatologies/%s/%s/%s/%s/CHELSA_%s_r1i1p1f1_w5e5_%s_%s_%s_%s_%s_norm.tif",
                         base_url, time_period, toupper(gcm), ssp, var_name, month_str,
                         tolower(gcm), ssp, var_name, month_str,
                         gsub("-", "_", time_period))
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      } else {
        # Dati storici mensili
        for (month in 1:12) {
          month_str <- sprintf("%02d", month)
          var_name <- ifelse(var == "pet_penman", "pet/pet_penman", var)
          url <- sprintf("%s/climatologies/1981-2010/%s/CHELSA_%s_%s_1981-2010_V.2.1.tif",
                         base_url, var_name, 
                         gsub(".*/", "", var_name), month_str)
          dest_file <- file.path(temp_dir, basename(url))
          if (download_file(url, dest_file)) {
            downloaded_files <- c(downloaded_files, dest_file)
          }
        }
      }
    }
    # Gestione dati annuali (es. swb)
    else if (var == "swb_annual") {
      # Solo dati storici disponibili (1981-2018)
      if (is.null(year)) year <- 2018  # Default all'ultimo anno disponibile
      
      url <- sprintf("%s/annual/swb/CHELSA_swb_%d_V.2.1.tif", base_url, year)
      dest_file <- file.path(temp_dir, basename(url))
      if (download_file(url, dest_file)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
    else {
      cli::cli_warn("Variable {.val {var}} not recognized for CHELSA source")
    }
  }
  
  return(downloaded_files)
}