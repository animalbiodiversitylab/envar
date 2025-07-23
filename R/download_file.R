# R/download_file.R
#' Download file with progress
#' @noRd
download_file <- function(url, dest_file, max_retries = 3) {
  # Il messaggio di avvio viene mostrato una sola volta
  cli::cli_progress_step("Downloading {.file {basename(dest_file)}}")
  
  for (i in 1:max_retries) {
    # Definiamo un User-Agent per simulare un browser e non essere bloccati
    user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    
    success <- FALSE # Flag per monitorare il successo del download
    
    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent(user_agent_string), # <-- MODIFICA CHIAVE: Aggiunta dello User-Agent
        httr::write_disk(dest_file, overwrite = TRUE),
        httr::progress(),
        httr::timeout(600)  # Timeout di 10 minuti (3600 era 1 ora)
      )
      
      # Controlliamo se la risposta del server è "200 OK"
      if (httr::status_code(response) == 200) {
        success <- TRUE # Il download è andato a buon fine
      } else {
        # Se il server risponde con un errore (es. 403, 404, 500), lo segnaliamo
        cli::cli_alert_warning("Tentativo {i}/{max_retries} fallito. Il server ha risposto con codice: {httr::status_code(response)}")
      }
    }, error = function(e) {
      # Questa parte intercetta errori di connessione, timeout, etc.
      cli::cli_alert_warning("Tentativo {i}/{max_retries} fallito con errore di connessione: {e$message}")
    })
    
    # Se il download ha avuto successo, usciamo dalla funzione
    if (success) {
      cli::cli_progress_done() # Finalizza la barra di progresso con successo
      return(TRUE)
    }
    
    # Se il download è fallito e non abbiamo esaurito i tentativi, aspettiamo prima di riprovare
    if (i < max_retries) {
      wait_time <- 2^i # Attesa esponenziale (2, 4, 8 secondi...)
      cli::cli_alert_info("Attendo {wait_time}s prima di riprovare...")
      Sys.sleep(wait_time)
    }
  }
  
  # Se tutti i tentativi sono falliti, lo segnaliamo
  cli::cli_progress_done(result = "failed") # Finalizza la barra di progresso come fallita
  cli::cli_alert_danger("Download di {.file {basename(dest_file)}} fallito dopo {max_retries} tentativi.")
  
  return(FALSE)
}