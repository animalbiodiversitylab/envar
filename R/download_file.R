# R/download_file.R
#' Download file with progress
#' @noRd
download_file <- function(url, dest_file, max_retries = 3) {
  cli::cli_progress_step("Downloading {.file {basename(dest_file)}}")
  
  for (i in 1:max_retries) {
    tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(dest_file, overwrite = TRUE),
        httr::progress(),
        httr::timeout(3600)  # 10 minute timeout
      )
      
      if (httr::status_code(response) == 200) {
        return(TRUE)
      }
    }, error = function(e) {
      if (i < max_retries) {
        cli::cli_alert_warning("Retry {i}/{max_retries} for {basename(dest_file)}")
        Sys.sleep(2^i)  # Exponential backoff
      } else {
        cli::cli_alert_danger("Failed to download {basename(dest_file)}: {e$message}")
        return(FALSE)
      }
    })
  }
  
  return(FALSE)
}