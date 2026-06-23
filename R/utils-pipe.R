#' Pipe operator
#'
#' Re-exported from \pkg{dplyr}. See \code{dplyr::\link[dplyr:reexports]{\%>\%}}
#' for details. This lets users chain \code{envar} functions, e.g.
#' \code{par_set(country = "Italy") \%>\% worldclim(vars = "bio1")}.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling \code{rhs(lhs)}.
NULL
