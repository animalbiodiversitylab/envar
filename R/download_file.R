# R/download_file.R
#' Download file with progress
#'
#' Downloads a file to `dest_file`. When caching is enabled
#' (`options(envar.cache = TRUE)`, set by [par_set()]), a complete copy that is
#' already present at `dest_file` is reused instead of being downloaded again,
#' allowing an interrupted pipeline to resume. Downloads are written to a
#' temporary `.part` file and moved into place only on success, so any file left
#' at `dest_file` is guaranteed to be a complete download.
#' @noRd
download_file <- function(url, dest_file, max_retries = 2) {

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

    success <- FALSE # Flag to monitor download success

    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent(user_agent_string),
        httr::write_disk(part_file, overwrite = TRUE),
        httr::progress(),
        httr::config(
          connecttimeout = 500,      # sec to establish connection
          timeout = 0,              # No limit on total time (0 = infinite)
          low_speed_limit = 0,    # At least 0 bytes/sec
          low_speed_time = 500      # For x seconds before giving up
        )
      )

      # Check if server response is "200 OK"
      if (httr::status_code(response) == 200) {
        # Move the completed download into its final location atomically.
        if (file.exists(dest_file)) try(fs::file_delete(dest_file), silent = TRUE)
        moved <- tryCatch(
          {
            fs::file_move(part_file, dest_file)
            TRUE
          },
          error = function(e) isTRUE(file.rename(part_file, dest_file))
        )
        success <- isTRUE(moved) # Download was successful
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

  # If all attempts fail, remove any leftover partial download
  if (file.exists(part_file)) try(fs::file_delete(part_file), silent = TRUE)

  cli::cli_progress_done(result = "failed")
  cli::cli_alert_danger("Download of {.file {basename(dest_file)}} failed after {max_retries} attempts.")

  return(FALSE)
}
