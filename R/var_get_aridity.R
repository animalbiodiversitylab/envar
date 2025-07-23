# R/var_get_aridity.R
#' Download Global Aridity Index and PET based on variable and month
#' @noRd
var_get_aridity <- function(bbox, resolution, variables, temp_dir, month, ...) {
  base_url <- "https://figshare.com/ndownloader/files/"
  
  # Mappa che definisce la logica di download come da specifiche.
  # Ogni variabile punta a una configurazione per dati 'mensili' e/o 'annuali'.
  aridity_map <- list(
    "aridity_index" = list(
      # Da usare solo se 'month' è specificato
      monthly = list(file_id = "34377269", pattern = "ai_v3_%02d.tif"),
      # Da usare solo se 'month' NON è specificato
      annual = list(file_id = "34377245", pattern = "ai_v3_yr.tif")
    ),
    "evapotranspiration" = list(
      # Da usare solo se 'month' è specificato
      monthly = list(file_id = "34377239", pattern = "et0_v3_%02d.tif"),
      # Da usare solo se 'month' NON è specificato
      annual = list(file_id = "34377245", pattern = "et0_v3_yr.tif")
    ),
    "evapotranspiration_sd" = list(
      # Questa variabile è disponibile SOLO come annuale
      annual = list(file_id = "34377245", pattern = "et0_v3_yr_sd.tif")
    )
  )
  
  downloaded_files <- character()
  # Tiene traccia dei file zip già scaricati per evitare download multipli
  # (utile per i dati annuali che sono nello stesso zip)
  processed_zips <- list()
  
  for (var in variables) {
    var_map <- aridity_map[[var]]
    if (is.null(var_map)) {
      cli::cli_warn("Variabile {.val {var}} non disponibile per la fonte 'aridity'. Salto.")
      next
    }
    
    config <- NULL
    target_filename <- NULL
    
    # --- INIZIO LOGICA DI SELEZIONE ---
    if (!is.null(month)) {
      # CASO 1: L'utente ha richiesto un mese specifico
      if (!month %in% 1:12) {
        cli::cli_abort("{.val {month}} non è un mese valido. Fornire un numero da 1 a 12.")
      }
      if (is.null(var_map$monthly)) {
        cli::cli_warn("Dati mensili non disponibili per {.val {var}}. Salto.")
        next # Passa alla prossima variabile
      }
      config <- var_map$monthly
      target_filename <- sprintf(config$pattern, month)
      
    } else {
      # CASO 2: L'utente NON ha richiesto un mese, quindi vuole i dati annuali
      if (is.null(var_map$annual)) {
        cli::cli_warn("Dati annuali non disponibili per {.val {var}}. Specificare un 'month' per questa variabile. Salto.")
        next # Passa alla prossima variabile
      }
      config <- var_map$annual
      target_filename <- config$pattern
    }
    # --- FINE LOGICA DI SELEZIONE ---
    
    # Se non è stata trovata una configurazione valida, salta
    if (is.null(config)) next
    
    url <- paste0(base_url, config$file_id)
    # Usa l'ID del file per il nome dello zip, così non viene riscaricato
    zip_filename <- file.path(temp_dir, paste0("figshare_", config$file_id, ".zip"))
    unzip_dir <- file.path(temp_dir, paste0("figshare_", config$file_id))
    
    # Controlla se il file zip è già stato scaricato e decompresso in questa sessione
    if (!config$file_id %in% names(processed_zips)) {
      if (download_file(url, zip_filename)) {
        fs::dir_create(unzip_dir)
        unzip(zip_filename, exdir = unzip_dir)
        processed_zips[[config$file_id]] <- unzip_dir # Marca come processato
      } else {
        cli::cli_warn("Download fallito per l'archivio ZIP: {.url {url}}")
        next # Salta questa variabile se il download fallisce
      }
    }
    
    # Cerca il file TIF corretto nella cartella decompressa
    all_tifs <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
    target_tif <- all_tifs[endsWith(all_tifs, target_filename)]
    
    if (length(target_tif) == 1) {
      downloaded_files <- c(downloaded_files, target_tif)
    } else {
      cli::cli_warn("Impossibile trovare il file {.file {target_filename}} dopo la decompressione.")
    }
  }
  
  return(downloaded_files)
}