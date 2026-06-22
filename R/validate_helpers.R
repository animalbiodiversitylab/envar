# R/validate_helpers.R
#
# Small internal helpers used across the package to turn cryptic failures
# (empty subsets, invalid CRS codes, missing arguments, ...) into short,
# precise, actionable error messages.

#' Suggest close matches for a mistyped value
#'
#' Tries, in order: case-insensitive substring matches, then approximate
#' (fuzzy) matches via [agrep()]. Used to power "Did you mean ...?" hints.
#'
#' @param value The (possibly mistyped) user input.
#' @param choices Character vector of valid values.
#' @param n Maximum number of suggestions to return.
#' @return Character vector of up to `n` suggested values (possibly empty).
#' @noRd
suggest_matches <- function(value, choices, n = 5) {
  value <- as.character(value)[1]
  choices <- unique(as.character(choices))
  choices <- choices[!is.na(choices) & nzchar(choices)]
  if (length(choices) == 0 || is.na(value) || !nzchar(value)) return(character(0))

  lv <- tolower(value)
  lc <- tolower(choices)

  # 1) the typed value is contained in a valid option (or vice versa)
  hits <- choices[grepl(lv, lc, fixed = TRUE) |
                    vapply(lc, function(ch) grepl(ch, lv, fixed = TRUE), logical(1))]

  # 2) fall back to fuzzy matching
  if (length(hits) == 0) {
    hits <- tryCatch(
      agrep(value, choices, ignore.case = TRUE, value = TRUE, max.distance = 0.2),
      error = function(e) character(0)
    )
  }

  utils::head(unique(hits), n)
}

#' Abort on an invalid categorical choice, with suggestions
#'
#' Builds a compact, consistent error: what was wrong, a "did you mean" hint
#' when a close match exists, and either the full list of valid values (when
#' short) or a count plus a pointer to where the full list lives.
#'
#' @param arg Human-readable name of the argument (e.g. "ecoregion").
#' @param value The invalid value the user supplied.
#' @param choices Character vector of valid values.
#' @param reference Optional string (e.g. a URL via `{.url ...}`) pointing to
#'   the full list, used when there are too many values to print.
#' @param max_list Print the full list only when there are at most this many.
#' @noRd
cli_abort_choice <- function(arg, value, choices, reference = NULL, max_list = 25) {
  choices <- unique(as.character(choices))
  choices <- choices[!is.na(choices) & nzchar(choices)]
  sugg <- suggest_matches(value, choices)
  n_choices <- length(choices)

  bullets <- c("x" = "{.val {value}} is not a valid {arg}.")
  if (length(sugg) > 0) {
    bullets <- c(bullets, "i" = "Did you mean {.val {sugg}}?")
  }
  if (n_choices == 0) {
    # nothing to suggest from
  } else if (n_choices <= max_list) {
    bullets <- c(bullets, "i" = "Valid options: {.val {choices}}.")
  } else if (!is.null(reference)) {
    bullets <- c(bullets, "i" = paste0("{n_choices} options are available - see ", reference, " for the full list."))
  } else {
    bullets <- c(bullets, "i" = "{n_choices} options are available (showing {max_list}): {.val {utils::head(choices, max_list)}}.")
  }

  cli::cli_abort(c("Invalid {arg}.", bullets))
}
