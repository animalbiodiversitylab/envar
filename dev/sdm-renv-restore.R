# =============================================================================
# sdm-renv-restore.R  --  USER SCRIPT (run by anyone reproducing the tutorial)
# -----------------------------------------------------------------------------
# Installs the exact package versions used in the Species Distribution Modelling
# vignette (vignettes/sdm.Rmd), as recorded in vignettes/renv.lock, so the
# tutorial code runs identically to the published version.
#
# HOW TO RUN
#   Open R with the working directory set to the package root (the folder that
#   contains the DESCRIPTION file), then run ONE of the two options below.
# =============================================================================

# Make sure a concrete CRAN mirror is available.
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

# -----------------------------------------------------------------------------
# OPTION A (recommended) -- isolated, project-local library.
# Creates an renv library inside the project and installs the pinned versions
# there, WITHOUT touching the packages in your main R library. Restart R after
# this so the project library is on .libPaths().
# -----------------------------------------------------------------------------
renv::activate(project = ".")
renv::restore(lockfile = "vignettes/renv.lock", prompt = FALSE)

# -----------------------------------------------------------------------------
# OPTION B -- install the pinned versions straight into your current library.
# Simpler, but it overwrites your existing versions of these packages.
# Use this instead of Option A if you do not want a project-local library.
#
#   renv::restore(
#     lockfile = "vignettes/renv.lock",
#     library  = .libPaths()[1],
#     prompt   = FALSE
#   )
# -----------------------------------------------------------------------------

# Note: 'envar' itself is not in the lockfile (the repository you are in IS the
# envar package). Install it from this repo, e.g. devtools::install() or
# remotes::install_github("animalbiodiversitylab/envar").
