# R/ssp_rcp.R

#' Convert an RCP forcing level to its two-digit code
#'
#' CMIP5/CMIP6 file names encode the Representative Concentration Pathway as a
#' two-digit integer (e.g. 8.5 W/m^2 -> "85", 2.6 -> "26"). This helper accepts
#' the forcing level either as a decimal (e.g. `8.5`) or as the already-scaled
#' code (e.g. `85`) and always returns the two-digit string.
#'
#' @param rcp Numeric or character vector of RCP forcing levels.
#' @return Character vector of two-digit RCP codes.
#' @noRd
rcp_code <- function(rcp) {
  vapply(as.numeric(rcp), function(r) {
    if (is.na(r)) return(NA_character_)
    # Decimal form (e.g. 8.5) -> 85; already-scaled form (e.g. 85) -> 85
    if (r < 10) {
      sprintf("%02d", as.integer(round(r * 10)))
    } else {
      sprintf("%d", as.integer(round(r)))
    }
  }, character(1))
}

#' Combine SSP and RCP into CMIP6 scenario codes
#'
#' CHELSA and WorldClim future projections identify scenarios with a combined
#' Shared Socioeconomic Pathway + Representative Concentration Pathway code
#' (e.g. SSP5-8.5 -> "585"). This helper builds those codes from the separate
#' `ssp` and `rcp` arguments, so that, for example, `ssp = 5` and `rcp = 8.5`
#' yield `"585"`.
#'
#' @param ssp Numeric or character vector of SSP families (e.g. `1`, `2`, `5`).
#'   A leading "ssp" prefix is tolerated and stripped. When `rcp` is `NULL` the
#'   values are assumed to already encode the full scenario (e.g. `"585"`) and
#'   are returned unchanged.
#' @param rcp Numeric or character vector of RCP forcing levels, given as a
#'   decimal (e.g. `8.5`, `4.5`, `2.6`) or as the already-scaled two-digit code
#'   (e.g. `85`). May be `NULL`.
#' @return Character vector of unique scenario codes, without the "ssp" prefix
#'   (e.g. `"585"`). Every requested `ssp` is paired with every requested `rcp`.
#' @noRd
combine_ssp_rcp <- function(ssp, rcp = NULL) {
  if (is.null(ssp)) return(NULL)

  ssp_chr <- sub("^ssp", "", tolower(as.character(ssp)))

  if (is.null(rcp)) {
    return(unique(ssp_chr))
  }

  rcp_chr <- rcp_code(rcp)

  codes <- character(0)
  for (s in ssp_chr) {
    for (r in rcp_chr) {
      codes <- c(codes, paste0(s, r))
    }
  }
  unique(codes)
}
