# R/topography.R

#' Scarica i dati topografici da EarthEnv
#'
#' Questa funzione costruisce l'URL e scarica i dati topografici da EarthEnv
#' in base a una combinazione di variabili, risoluzione, aggregazione e fonte.
#' Utilizza la funzione di supporto `download_file` per gestire il download.
#'
#' @param bbox Bounding box (vettore numerico). Non utilizzato per la costruzione dell'URL
#'   in questo caso, ma mantenuto per coerenza con altre funzioni `var_get_*`.
#' @param resolution Risoluzione spaziale (es. "1KM", "5KM").
#' @param variables Un vettore di caratteri delle variabili da scaricare (es. "elevation", "tpi").
#' @param temp_dir Directory temporanea dove salvare i file scaricati.
#' @param topo_source La fonte dei dati topografici. Deve essere "GMTED" o "SRTM".
#' @param aggregation Il metodo di aggregazione (es. "md", "mn", "sd").
#' @param ... Argomenti aggiuntivi (attualmente non utilizzati).
#'
#' @return Un vettore di caratteri contenente i percorsi completi dei file scaricati con successo.
#' @noRd
topography <- function(x, variables, algorithm, topo_source) {

  par_list = get_par(x)
  
  if (inherits(par_list[[1]],"SpatRaster")) {
    
    grid = par_list$grid
    mask = par_list$mask
    res = par_list$res
   
    
  } else {
    
  points <- par_list$mask
  bbox_points <- par_list$bbox
  
  }
  
  # Vettore per memorizzare i percorsi dei file scaricati
  downloaded_files <- character()
  
  # URL di base per tutti i dati topografici di EarthEnv
  base_url <- "https://data.earthenv.org/topography/"

  # --- 1. Validazione e Normalizzazione dell'Input ---
  
  source_upper <- toupper(topo_source)
  valid_sources <- c("GMTED", "SRTM")
  
  if (!source_upper %in% valid_sources) {
    cli::cli_abort("Il parametro `topo_source` deve essere uno tra: {.val {paste(valid_sources, collapse = ', ')}}")
  }
  
  # --- 2. Loop sulle Variabili e Download ---
  
  for (var in variables) {
    if (var == "elevation") {
      if (algorithm == "max") {
        algorithm = "ma"
      }
    }
    
    # --- 2a. Costruzione Dinamica del Nome del File ---
  
    source_filename_part <- if (source_upper == "GMTED") {
      paste0(source_upper, algorithm)
    } else {
      source_upper
    }
    
    # Assembla il nome del file completo seguendo lo schema:
    # {variabile}_{risoluzione}{algoritmo}_{parte_fonte}.tif

    file_name <- paste0(var, "_1KM", algorithm, "_", source_filename_part, ".tif")
    
    # Costruisce l'URL completo e il percorso di destinazione
    url <- paste0(base_url, file_name)
    
    temp_dir <- fs::path_temp("envar/grids")
    fs::dir_create(temp_dir)
    
    dest_file <- file.path(temp_dir, file_name)
    
    # --- 2b. Chiamata alla Funzione di Download ---
    
    if (download_file(url, dest_file)) {
      downloaded_files <- c(downloaded_files, dest_file)
    } else {
      cli::cli_alert_warning("Impossibile scaricare la variabile {.val {var}} da {.url {url}}.")
    }
  }
  

  if (inherits(par_list[[1]],"SpatRaster")){
    
    processed_stack <- process_layers(
      files = downloaded_files, target_grid = grid, mask = mask,res=res
    )
    
    if (inherits(x, "SpatRaster")) {
      processed_stack <- c(x, processed_stack)
      
    }
    
    # Restituisce il vettore di percorsi ai file scaricati con successo.
    # La funzione `var_get` principale si occuperà di processare questi file.
    return(processed_stack)
 
    
  } else {
    
    extracted <- data.frame(process_points(file = downloaded_files, points = points))
    
    if (inherits(x, "data.frame")) {
      extracted <- data.frame(merge(x, extracted[, c(1, 4)], by = "ID"))
      
    }
    
    return(extracted)
    
 
  
  }
  
}
