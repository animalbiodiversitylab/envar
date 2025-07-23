# R/var_get_cloud.R
#' @title Scarica i dati sulla copertura nuvolosa da EarthEnv
#' @description
#' Questa funzione scarica vari prodotti di dati sulla copertura nuvolosa dal repository
#' EarthEnv. Rileva automaticamente le richieste per la media annuale, tutti i mesi,
#' o mesi specifici (es. "cloud_monthly_09").
#'
#' @param bbox Un oggetto che definisce l'estensione spaziale (attualmente non utilizzato).
#' @param resolution La risoluzione dei dati (attualmente non utilizzata).
#' @param variables Un vettore di caratteri che specifica quali variabili scaricare.
#'        Le opzioni valide includono:
#'        - "cloud_annual": Copertura nuvolosa media annuale.
#'        - "cloud_monthly": Scarica tutte le 12 medie mensili.
#'        - "cloud_monthly_01", "cloud_monthly_02", ...: Scarica un mese specifico.
#'        - "cloud_intraannual_sd": Deviazione standard intra-annuale.
#'        - "cloud_interannual_sd": Deviazione standard inter-annuale.
#'        - ... (e tutte le altre variabili definite in precedenza)
#' @param temp_dir La directory temporanea in cui salvare i file scaricati.
#' @param ... Argomenti aggiuntivi passati a download_file.
#' @return Un vettore di caratteri contenente i percorsi dei file scaricati.
#' @noRd

var_get_cloud <- function(bbox, resolution, variables, temp_dir, ...) {
  base_url <- "https://data.earthenv.org/cloud/"
  downloaded_files <- character()
  
  # Funzione helper per il download (presumendo che esista)
  # download_file <- function(url, dest_file, ...) { ... }
  
  # Mappa per le variabili con corrispondenza uno-a-uno tra nome variabile e nome file
  variable_map <- c(
    "cloud_annual"                  = "MODCF_meanannual.tif",
    "cloud_intraannual_sd"          = "MODCF_intraannualSD.tif",
    "cloud_interannual_sd"          = "MODCF_interannualSD.tif",
    "cloud_forest_prediction"       = "MODCF_CloudForestPrediction.tif",
    "cloud_seasonality_concentration" = "MODCF_seasonality_concentration.tif",
    "cloud_seasonality_rgb"         = "MODCF_seasonality_rgb.tif",
    "cloud_seasonality_theta"       = "MODCF_seasonality_theta.tif",
    "cloud_spatial_sd"              = "MODCF_spatialSD_1deg.tif"
  )
  
  # Mappa per i nomi dei file di destinazione locali
  dest_file_map <- c(
    "cloud_annual"                  = "cloud_mean_annual.tif",
    "cloud_intraannual_sd"          = "cloud_intraannual_sd.tif",
    "cloud_interannual_sd"          = "cloud_interannual_sd.tif",
    "cloud_forest_prediction"       = "cloud_forest_prediction.tif",
    "cloud_seasonality_concentration" = "cloud_seasonality_concentration.tif",
    "cloud_seasonality_rgb"         = "cloud_seasonality_rgb.tif",
    "cloud_seasonality_theta"       = "cloud_seasonality_theta.tif",
    "cloud_spatial_sd"              = "cloud_spatial_sd_1deg.tif"
  )
  
  # 1. Gestisce le variabili con mappatura diretta
  vars_to_download <- intersect(variables, names(variable_map))
  
  for (var in vars_to_download) {
    url <- paste0(base_url, variable_map[var])
    dest_file <- file.path(temp_dir, dest_file_map[var])
    # Passa '...' alla funzione di download
    if (download_file(url, dest_file, ...)) {
      downloaded_files <- c(downloaded_files, dest_file)
    }
  }
  
  # 2. Logica per la copertura nuvolosa mensile (Migliorata)
  monthly_vars <- grep("cloud_monthly", variables, value = TRUE)
  
  if (length(monthly_vars) > 0) {
    months_to_download <- integer()
    
    # Caso 1: L'utente vuole tutti i 12 mesi
    if ("cloud_monthly" %in% monthly_vars) {
      months_to_download <- 1:12
    } else {
      # Caso 2: L'utente vuole mesi specifici (es. "cloud_monthly_09")
      # Estrae il numero del mese e valida che sia tra "01" e "12"
      month_num_str <- sub("cloud_monthly_", "", monthly_vars)
      valid_months <- month_num_str[grepl("^(0[1-9]|1[0-2])$", month_num_str)]
      if(length(valid_months) > 0) {
        months_to_download <- c(months_to_download, as.integer(valid_months))
      }
    }
    
    # Rimuove duplicati e scarica i file
    for (month in unique(months_to_download)) {
      url <- sprintf("%sMODCF_monthlymean_%02d.tif", base_url, month)
      dest_file <- file.path(temp_dir, sprintf("cloud_month_%02d.tif", month))
      # Passa '...' alla funzione di download
      if (download_file(url, dest_file, ...)) {
        downloaded_files <- c(downloaded_files, dest_file)
      }
    }
  }
  
  return(unique(downloaded_files))
}