# R/var_get_worldclim.R
#' @title Scarica dati WorldClim v2.1
#' @description Scarica le variabili climatiche o bioclimatiche da WorldClim 2.1,
#' gestendo il download di file ZIP e la loro estrazione.
#'
#' @param resolution Risoluzione dei dati. Valori possibili: "10m", "5m", "2.5m", "30s".
#' @param variables Un vettore di stringhe che specifica le variabili da scaricare.
#'        Per le variabili bioclimatiche usare "bio". Per le variabili mensili
#'        usare "tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr".
#' @param temp_dir La directory temporanea dove scaricare ed estrarre i file.
#'
#' @return Un vettore di caratteri contenente i percorsi dei file .tif scaricati ed estratti.
#' @noRd
#' @importFrom utils download.file unzip

var_get_worldclim <- function(resolution, variables, temp_dir, ...) {
  # URL di base per i dati WorldClim versione 2.1
  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
  
  # Mappatura della risoluzione per l'URL.
  # Nota: le risoluzioni più alte come 1km non sono direttamente disponibili come ZIP
  # e richiederebbero un'ulteriore elaborazione (ricampionamento) non inclusa qui.
  res_map <- list(
    "30s" = "30s",
    "2.5m" = "2-5m", # La mappatura corretta per l'URL è "2-5m"
    "5m" = "5m",
    "10m" = "10m",
    "1km" = "30s"
  )
  
  wc_res <- res_map[[resolution]]
  if (is.null(wc_res)) {
    stop("Risoluzione non valida. Scegliere tra: '10m', '5m', '2.5m', '30s'.")
  }
  
  # Vettore per memorizzare i percorsi dei file TIF finali
  all_extracted_files <- character()
  
  # Itera su ogni variabile richiesta (es. "bio", "tavg")
  for (var in variables) {
    # Costruisce il nome del file ZIP. Per le variabili bioclimatiche è 'bio', 
    # per le altre (tavg, prec, etc.) è il nome della variabile stessa.
    var_name_in_zip <- if (var == "bioclim") "bio" else var
    
    zip_filename <- sprintf("wc2.1_%s_%s.zip", wc_res, var_name_in_zip)
    url <- file.path(base_url, zip_filename)
    
    # Percorso di destinazione per il file ZIP nella directory temporanea
    zip_dest_file <- file.path(temp_dir, basename(url))
    
    cat(sprintf("Tentativo di download per la variabile '%s' da: %s\n", var, url))
    
    # Scarica il file ZIP, gestendo eventuali errori
    download_result <- try(
      download_file(url, dest_file = zip_dest_file),
      silent = FALSE
    )

    if (inherits(download_result, "try-error")) {
      warning(sprintf("Download fallito per l'URL: %s. La variabile '%s' sarà saltata.", url, var))
      next # Passa alla variabile successiva
    }
    
    cat(sprintf("Download completato. Estrazione di: %s\n", zip_dest_file))
    
    # Decomprime il file nella directory temporanea
    # 'list = TRUE' per ottenere i nomi dei file contenuti
    zip_contents <- unzip(zip_dest_file, list = TRUE)
    
    # Estrae effettivamente i file
    unzip(zip_dest_file, exdir = temp_dir, junkpaths = TRUE)
    
    # Ottiene i percorsi completi dei file .tif estratti
    # 'junkpaths = TRUE' mette tutti i file direttamente in temp_dir
    extracted_tifs <- file.path(temp_dir, zip_contents$Name)
    
    # Filtra per assicurarsi di avere solo file .tif che esistono
    extracted_tifs <- extracted_tifs[grepl("\\.tif$", extracted_tifs) & file.exists(extracted_tifs)]
    
    if (length(extracted_tifs) > 0) {
      cat(sprintf("Estratti %d file .tif per la variabile '%s'.\n", length(extracted_tifs), var))
      all_extracted_files <- c(all_extracted_files, extracted_tifs)
    } else {
      warning(sprintf("Nessun file .tif trovato dopo l'estrazione per la variabile '%s'.", var))
    }
  }
  
  # Restituisce la lista completa dei percorsi ai file .tif
  return(all_extracted_files)
}