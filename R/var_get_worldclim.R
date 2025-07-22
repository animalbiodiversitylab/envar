#' @title Scarica dati WorldClim v2.1
#' @description Scarica le variabili climatiche o bioclimatiche da WorldClim 2.1,
#' estrae solo i layer richiesti, e restituisce i percorsi ai file `.tif`.
#'
#' @param bbox Bounding box in formato `terra::ext` o `sf` (usato per compatibilità).
#' @param resolution Risoluzione dei dati: "10m", "5m", "2.5m", "30s", "1km".
#' @param variables Variabili da scaricare: "bioclim", "tavg", "prec", "tmin", etc.
#' @param layer_names (opzionale) Nomi specifici dei layer da selezionare, ad esempio "bio1", "bio12".
#' @param temp_dir Directory temporanea dove scaricare ed estrarre i file.
#' @param ... Argomenti aggiuntivi (ignorati).
#'
#' @return Vettore con i percorsi ai file `.tif` richiesti.
#' @noRd

var_get_worldclim <- function(bbox,
                              resolution,
                              variables,
                              layer_names = NULL,
                              temp_dir,
                              ...) {
  base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base"
  
  res_map <- list(
    "30s" = "30s",
    "2.5m" = "2-5m",
    "5m" = "5m",
    "10m" = "10m",
    "1km" = "30s" # proxy
  )
  
  wc_res <- res_map[[resolution]]
  if (is.null(wc_res)) {
    stop("Risoluzione non valida. Scegli tra: '10m', '5m', '2.5m', '30s', '1km'.")
  }
  
  all_selected_files <- character()
  
  for (var in variables) {
    var_in_url <- if (var == "bioclim") "bio" else var
    zip_filename <- sprintf("wc2.1_%s_%s.zip", wc_res, var_in_url)
    url <- file.path(base_url, zip_filename)
    zip_dest <- file.path(temp_dir, basename(url))
    
    cat(sprintf("Scaricando '%s' da: %s\n", var, url))
    
    download_result <- try(
      download_file(url, dest_file = zip_dest),
      silent = FALSE
    )
    
    if (inherits(download_result, "try-error")) {
      warning(sprintf("Download fallito per: %s", url))
      next
    }
    
    # Lista contenuti dello ZIP
    zip_contents <- unzip(zip_dest, list = TRUE)$Name
    tif_files <- zip_contents[grepl("\\.tif$", zip_contents)]
    
    # Estrae solo i .tif
    unzip(zip_dest, exdir = temp_dir, files = tif_files, junkpaths = TRUE)
    extracted_paths <- file.path(temp_dir, basename(tif_files))
    # Filtra solo i layer richiesti se specificati (es. "bio1.tif", "tavg_01.tif")
    if (!is.null(layer_names)) {
      layer_patterns <- paste0("^(", paste(layer_names, collapse = "|"), ")\\.tif$")
      extracted_paths <- extracted_paths[grepl(layer_patterns, basename(extracted_paths))]
    }
    
    if (length(extracted_paths) > 0) {
      cat(sprintf("Selezionati %d file per la variabile '%s'.\n", length(extracted_paths), var))
      all_selected_files <- c(all_selected_files, extracted_paths)
    } else {
      warning(sprintf("Nessun layer valido trovato per '%s'", var))
    }
  }
  
  return(all_selected_files)
}
