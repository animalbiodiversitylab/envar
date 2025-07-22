# R/cleanup_temp.R
#' Clean up temporary files
#' @noRd
cleanup_temp <- function(temp_dir) {
  if (fs::dir_exists(temp_dir)) {
    cli::cli_progress_step("Cleaning up temporary files...")
    fs::dir_delete(temp_dir)
  }
}