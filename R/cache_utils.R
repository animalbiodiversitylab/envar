# R/cache_utils.R

#' Directory used to store downloaded grid files
#'
#' When caching is enabled (the default, controlled by
#' `options(envar.cache = TRUE)` which is set by [par_set()] via its `cache`
#' argument), downloaded source rasters are kept in a persistent per-user cache
#' directory. This way an interrupted download pipeline can be re-launched and
#' will resume from where it stopped, reusing files that were already retrieved
#' instead of downloading them again.
#'
#' When caching is disabled, a temporary session directory is used instead, and
#' its files are removed when the R session ends.
#'
#' @return Path to the directory (created if necessary).
#' @noRd
envar_grids_dir <- function() {
  use_cache <- isTRUE(getOption("envar.cache", TRUE))

  dir <- NULL
  if (use_cache) {
    # tools::R_user_dir() is available on R >= 4.0; fall back to a temp dir
    # on older versions so the package still works.
    dir <- tryCatch(
      file.path(tools::R_user_dir("envar", which = "cache"), "grids"),
      error = function(e) NULL
    )
  }

  if (is.null(dir)) {
    dir <- fs::path_temp("envar/grids")
  }

  fs::dir_create(dir)
  dir
}

#' Clear the envar download cache
#'
#' Removes all files stored in the persistent download cache used when
#' `cache = TRUE` in [par_set()]. This is useful to free disk space or to force
#' a fresh download of every variable.
#'
#' @return Invisibly, the path of the cache directory that was cleared.
#' @export
clear_cache <- function() {
  dir <- tryCatch(
    file.path(tools::R_user_dir("envar", which = "cache"), "grids"),
    error = function(e) fs::path_temp("envar/grids")
  )
  if (fs::dir_exists(dir)) {
    fs::dir_delete(dir)
    cli::cli_alert_success("Cleared envar download cache at {.file {dir}}.")
  } else {
    cli::cli_alert_info("No envar download cache found at {.file {dir}}.")
  }
  invisible(dir)
}
