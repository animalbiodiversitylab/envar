# R/download_file.R
#' Download file with progress
#' @noRd
download_file <- function(url, dest_file, max_retries = 2) {

  for (i in 1:max_retries) {
    # Simulate a browser
    user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    
    success <- FALSE # Flag to monitor download success
    
    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent(user_agent_string), 
        httr::write_disk(dest_file, overwrite = TRUE),
        httr::progress(),
        httr::config(
          connecttimeout = 60,      # 60 sec to establish connection
          timeout = 0,              # No limit on total time (0 = infinite)
          low_speed_limit = 100,    # At least 100 bytes/sec
          low_speed_time = 120      # For 120 seconds before giving up
        )
      )
      
      # Check if server response is "200 OK"
      if (httr::status_code(response) == 200) {
        success <- TRUE # Download was successful
      } else {
 
        cli::cli_alert_warning("Attempt {i}/{max_retries} failed")
      }
    }, error = function(e) {

      cli::cli_alert_warning("Attempt {i}/{max_retries} failed")
    })
    
    # If download was successfull, get out
    if (success) {
      cli::cli_progress_done() # Finalize progress bar
      return(TRUE)
    }
    
  
    if (i < max_retries) {
      wait_time <- 2^i # Wait between one request and another is exponential (2, 4, 8 seconds)
      cli::cli_alert_info("Waiting {wait_time}s before next attempt...")
      Sys.sleep(wait_time)
    }
  }
  
  # If all attempts fail
  cli::cli_progress_done(result = "failed") 
  cli::cli_alert_danger("Download of {.file {basename(dest_file)}} failed after {max_retries} attempts.")
  
  return(FALSE)
}
