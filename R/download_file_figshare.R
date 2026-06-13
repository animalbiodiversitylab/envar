# R/download_file_figshare.R
#' Download a file from Figshare with progress
#'
#' Downloads a file to `dest_file`. When caching is enabled
#' (`options(envar.cache = TRUE)`, set by [var_get()]), a complete copy already
#' present at `dest_file` is reused instead of being downloaded again, allowing
#' an interrupted pipeline to resume. Downloads are written to a temporary
#' `.part` file and moved into place only on success, so any file left at
#' `dest_file` is guaranteed to be a complete download. Follows Figshare
#' redirects and sends the headers Figshare expects.
#' @noRd
download_file_figshare <- function(url, dest_file, max_retries = 2) {
  
  # If caching is enabled and a complete copy already exists, reuse it.
  if (isTRUE(getOption("envar.cache", TRUE)) &&
      file.exists(dest_file) && isTRUE(file.info(dest_file)$size > 0)) {
    cli::cli_alert_success(
      "Using cached copy of {.file {basename(dest_file)}} (skipping download)."
    )
    return(TRUE)
  }
  
  # Download to a temporary part-file; only move it into place on success so
  # that a file present at dest_file is always a complete download.
  part_file <- paste0(dest_file, ".part")
  
  for (i in 1:max_retries) {
    # Simulate a browser
    user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"
    
    success <- FALSE
    
    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent(user_agent_string),
        httr::write_disk(part_file, overwrite = TRUE),
        httr::progress(),
        httr::config(
          followlocation = TRUE,     # Explicitly follow redirects
          maxredirs = 10,            # Allow up to 10 redirects
          connecttimeout = 60,       # Connection timeout
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
        # Verify the part-file was actually written and has content
        if (file.exists(part_file) && isTRUE(file.info(part_file)$size > 0)) {
          # Move the completed download into its final location atomically.
          if (file.exists(dest_file)) try(fs::file_delete(dest_file), silent = TRUE)
          moved <- tryCatch(
            {
              fs::file_move(part_file, dest_file)
              TRUE
            },
            error = function(e) isTRUE(file.rename(part_file, dest_file))
          )
          success <- isTRUE(moved)
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
    
    # Clean up partial download before retrying
    if (file.exists(part_file)) try(fs::file_delete(part_file), silent = TRUE)
    
    if (i < max_retries) {
      wait_time <- 2^i
      cli::cli_alert_info("Waiting {wait_time}s before next attempt...")
      Sys.sleep(wait_time)
    }
  }
  
  # If all attempts fail, remove any leftover partial download
  if (file.exists(part_file)) try(fs::file_delete(part_file), silent = TRUE)
  
  cli::cli_progress_done(result = "failed")
  cli::cli_alert_danger("Download of {.file {basename(dest_file)}} failed after {max_retries} attempts.")
  
  return(FALSE)
}