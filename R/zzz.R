# R/zzz.R
.onLoad <- function(libname, pkgname) {
  # Check for required packages
  required_pkgs <- c("terra", "sf", "rnaturalearth", "exactextractr")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    packageStartupMessage(
      "enviRget: Missing required packages: ", 
      paste(missing_pkgs, collapse = ", "),
      "\nPlease install with: install.packages(c('", 
      paste(missing_pkgs, collapse = "', '"), "'))"
    )
  }
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("If using the package for a publication please cite as in: citation('envar')")
  }
}
