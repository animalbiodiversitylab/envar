#' @keywords internal
"_PACKAGE"

# The package calls its dependencies with `::` throughout. `terra` and `usdm`
# are imported here so that these two heavier namespaces are loaded together
# with `envar` rather than on the first call that happens to need them.
#' @importFrom utils capture.output unzip write.csv
#' @importFrom terra rast
#' @importFrom usdm vif
NULL

# 'Europe' is a lazy-loaded dataset shipped with the package and used
# internally by process_extent(); register it to satisfy R CMD check.
utils::globalVariables("Europe")
