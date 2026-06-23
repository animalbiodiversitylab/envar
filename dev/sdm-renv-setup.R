# =============================================================================
# sdm-renv-setup.R  --  MAINTAINER / CREATOR SCRIPT (run once by the author)
# -----------------------------------------------------------------------------
# Captures the *currently installed* versions of the R packages used in the
# Species Distribution Modelling vignette (vignettes/sdm.Rmd) and writes them
# to a self-contained lockfile: vignettes/renv.lock
#
# This lockfile is what end users restore to reproduce the tutorial exactly.
# Re-run this script whenever you want to refresh the pinned versions to match
# your current library.
#
# HOW TO RUN
#   Open R with the working directory set to the package root (the folder that
#   contains the DESCRIPTION file), then:
#       source("vignettes/sdm-renv-setup.R")
#   or from a shell:
#       Rscript vignettes/sdm-renv-setup.R
# =============================================================================

# Make sure a concrete CRAN mirror is set so renv records a usable source.
local({
  repos <- getOption("repos")
  if (is.null(repos[["CRAN"]]) || repos[["CRAN"]] %in% c("@CRAN@", "")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
})

# Install renv itself if it is missing.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Packages directly used by the SDM tutorial. Their recursive dependencies are
# added to the lockfile automatically by renv::snapshot().
sdm_pkgs <- c(
  "envar",            # the package itself (pinned from its GitHub remote)
  "terra",
  "raster",
  "sf",
  "dismo",
  "spatialEco",
  "ENMeval",
  "ecospat",
  "PresenceAbsence"
)

# Sanity check: warn about anything not currently installed.
missing <- sdm_pkgs[!vapply(sdm_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("These tutorial packages are not installed, so they cannot be pinned: ",
       paste(missing, collapse = ", "),
       "\nInstall them first, then re-run this script.")
}

# Write the scoped lockfile from the versions currently on .libPaths().
renv::snapshot(
  project  = ".",
  lockfile = "vignettes/renv.lock",
  packages = sdm_pkgs,        # include exactly these + their recursive deps
  prompt   = FALSE,
  force    = TRUE
)

message("\nWrote vignettes/renv.lock with the current versions of the SDM tutorial packages.")
