#' Download file with progress
#' @noRd
download_file_figshare <- function(url, dest_file, max_retries = 2) {
  for (i in 1:max_retries) {
    # Simulate a browser
    user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    
    success <- FALSE
    
    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent(user_agent_string),
        httr::write_disk(dest_file, overwrite = TRUE),
        httr::progress(),
        httr::config(
          followlocation = TRUE,     # Explicitly follow redirects
          maxredirs = 10,            # Allow up to 10 redirects
          connecttimeout = 60,       # More reasonable connection timeout
          timeout = 0,               # No limit on total time
          low_speed_limit = 1000,    # At least 1KB/sec
          low_speed_time = 60        # For 60 seconds before giving up
        ),
        # Add headers that Figshare might expect
        httr::add_headers(
          "Accept" = "*/*",
          "Accept-Language" = "en-US,en;q=0.9",
          "Referer" = "https://figshare.com/"
        )
      )
      
      # Check if server response is successful (200-299 range)
      if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
        # Verify file was actually written and has content
        if (file.exists(dest_file) && file.info(dest_file)$size > 0) {
          success <- TRUE
        } else {
          cli::cli_alert_warning("Attempt {i}/{max_retries} failed: File not written or empty")
        }
      } else {
        cli::cli_alert_warning("Attempt {i}/{max_retries} failed: HTTP {httr::status_code(response)}")
      }
    }, error = function(e) {
      cli::cli_alert_warning("Attempt {i}/{max_retries} failed: {e$message}")
    })
    
    if (success) {
      cli::cli_progress_done()
      return(TRUE)
    }
    
    # Clean up partial download
    if (file.exists(dest_file)) {
      file.remove(dest_file)
    }
    
    if (i < max_retries) {
      wait_time <- 2^i
      cli::cli_alert_info("Waiting {wait_time}s before next attempt...")
      Sys.sleep(wait_time)
    }
  }
  
  cli::cli_progress_done(result = "failed")
  cli::cli_alert_danger("Download of {.file {basename(dest_file)}} failed after {max_retries} attempts.")
  
  return(FALSE)
}
