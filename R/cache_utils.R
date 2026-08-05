# R/cache_utils.R

#' Resolve whether the persistent download cache may be used
#'
#' The persistent cache lives in the per-user cache directory returned by
#' [tools::R_user_dir()], i.e. inside the user's home filespace. Following CRAN
#' policy, nothing is ever written there unless the user has explicitly agreed
#' to it. This helper turns the `cache` argument of [par_set()] into that
#' decision:
#'
#' * `TRUE`/`FALSE` are an explicit choice and are used as given;
#' * `NULL` (the default) means "ask": in an interactive session the user is
#'   prompted once per session, and in a non-interactive session (scripts,
#'   `R CMD check`, vignette building) the answer is always `FALSE`, so a
#'   session temporary directory is used instead.
#'
#' The resolved answer is stored in `options(envar.cache = )` and reused for the
#' rest of the session, so the question is asked at most once. Users who do not
#' want to be asked at all can set the option themselves beforehand.
#'
#' @param cache `TRUE`, `FALSE` or `NULL` (as passed to [par_set()]).
#' @return A single logical: whether the persistent cache may be used.
#' @noRd
resolve_cache_consent <- function(cache = NULL) {

  # Explicit user choice always wins.
  if (!is.null(cache)) {
    if (!is.logical(cache) || length(cache) != 1 || is.na(cache)) {
      cli::cli_abort("{.arg cache} must be {.code TRUE}, {.code FALSE} or {.code NULL}.")
    }
    options(envar.cache = cache)
    return(cache)
  }

  # A decision already taken in this session (or an option set by the user in
  # e.g. .Rprofile) is reused without asking again.
  previous <- getOption("envar.cache", NULL)
  if (!is.null(previous) && is.logical(previous) && length(previous) == 1 && !is.na(previous)) {
    return(previous)
  }

  # Non-interactive sessions are never asked and never write to the home
  # filespace: a session temporary directory is used.
  if (!interactive()) {
    options(envar.cache = FALSE)
    return(FALSE)
  }

  dir <- envar_cache_root()
  cli::cli_alert_info(c(
    "envar can keep the files it downloads in a persistent cache at {.file {dir}}, ",
    "so that an interrupted download pipeline resumes instead of downloading everything again."
  ))
  answer <- utils::askYesNo(
    "May envar store downloaded files in that directory?",
    default = FALSE
  )
  consent <- isTRUE(answer)

  if (consent) {
    cli::cli_alert_success(
      "Download cache is ON. Call {.fn clear_cache} to empty it, or use {.code cache = FALSE} to switch it off."
    )
  } else {
    cli::cli_alert_info(
      "Download cache is OFF: files are stored in a session temporary directory and removed when R exits."
    )
  }

  options(envar.cache = consent)
  consent
}

#' Root of the persistent per-user cache
#'
#' @return Path of the cache directory (not created by this function).
#' @noRd
envar_cache_root <- function() {
  # tools::R_user_dir() is available on R >= 4.0; fall back to a session
  # temporary directory on older versions so the package still works.
  tryCatch(
    file.path(tools::R_user_dir("envar", which = "cache"), "grids"),
    error = function(e) fs::path_temp("envar/grids")
  )
}

#' Directory used to store downloaded grid files
#'
#' When caching is enabled (controlled by `options(envar.cache = TRUE)`, which
#' [par_set()] sets from its `cache` argument after the user has agreed to it),
#' downloaded source rasters are kept in a persistent per-user cache directory.
#' This way an interrupted download pipeline can be re-launched and will resume
#' from where it stopped, reusing files that were already retrieved instead of
#' downloading them again.
#'
#' When caching is disabled - which is the case whenever the user has not
#' explicitly agreed to it, and always in non-interactive sessions - a temporary
#' session directory is used instead, and its files are removed when the R
#' session ends.
#'
#' @return Path to the directory (created if necessary).
#' @noRd
envar_grids_dir <- function() {
  use_cache <- isTRUE(getOption("envar.cache", FALSE))

  dir <- if (use_cache) envar_cache_root() else fs::path_temp("envar/grids")

  fs::dir_create(dir)
  dir
}

#' Clear the envar download cache
#'
#' Removes all files stored in the persistent download cache used when
#' `cache = TRUE` in [par_set()]. This is useful to free disk space or to force
#' a fresh download of every variable.
#'
#' The cache is stored in the per-user cache directory returned by
#' [tools::R_user_dir()] and is only ever written after the user has agreed to
#' it (see the `cache` argument of [par_set()]).
#'
#' @return Invisibly, the path of the cache directory that was cleared.
#' @examples
#' \donttest{
#' # Empty the persistent download cache, if the user enabled one
#' clear_cache()
#' }
#' @export
clear_cache <- function() {
  dir <- envar_cache_root()
  if (fs::dir_exists(dir)) {
    fs::dir_delete(dir)
    cli::cli_alert_success("Cleared envar download cache at {.file {dir}}.")
  } else {
    cli::cli_alert_info("No envar download cache found at {.file {dir}}.")
  }
  invisible(dir)
}
