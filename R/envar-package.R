#' @keywords internal
"_PACKAGE"

#' @importFrom utils capture.output unzip write.csv
NULL

# 'Europe' is a lazy-loaded dataset shipped with the package and used
# internally by process_extent(); register it to satisfy R CMD check.
utils::globalVariables("Europe")
