# R/zzz.R
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("If using the package for a publication please cite as in: citation('envar')")
  }
}
