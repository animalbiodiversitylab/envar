# R/provenance.R
#
# Provenance registry used by metadata().
#
# The registry is a single environment that lives in the package namespace and
# records, while a pipeline runs, everything that metadata() later needs to
# report: the settings given to par_set(), the datasets that were called with
# their arguments, every URL that was actually downloaded (with its timestamp)
# and the native resolution of every source raster that was processed.
#
# It is filled by tiny calls placed in par_set(), in the dataset functions, in
# download_file()/download_file_figshare() and in process_raster_layer()/
# process_points(), so that no dataset function has to know anything about
# metadata reporting. par_set() clears it, which means the registry always
# describes the pipeline that started with the most recent par_set() call.

.envar_prov <- new.env(parent = emptyenv())

#' Empty the provenance registry and record the study-area settings
#'
#' Called at the very beginning of [par_set()], so that everything downloaded
#' afterwards (including the study-area boundaries themselves) is attributed to
#' the pipeline being started.
#'
#' @param settings Named list of the arguments par_set() was called with.
#' @return Invisibly, `NULL`.
#' @noRd
prov_reset <- function(settings = list()) {
  .envar_prov$started <- Sys.time()
  .envar_prov$par <- settings
  .envar_prov$datasets <- list()
  .envar_prov$current <- NULL
  .envar_prov$boundary <- list(downloads = list(), native = list())
  invisible(NULL)
}

#' Update some of the recorded par_set() settings
#'
#' [par_set()] resolves a few of its arguments (the resolution default and the
#' cache consent) after the registry has been created; this replaces them.
#' @noRd
prov_update_par <- function(...) {
  if (!prov_active()) return(invisible(NULL))
  values <- list(...)
  for (nm in names(values)) .envar_prov$par[[nm]] <- values[[nm]]
  invisible(NULL)
}

#' Capture the evaluated arguments of a function call
#'
#' Reads the formals of `fun` from the frame `env` in which they were evaluated.
#' The pipeline object `x` and `...` are skipped (`x` can be a large raster and
#' is described separately by metadata()), and objects that are not simple
#' values, such as an `sf` study area, are stored as a class placeholder rather
#' than copied.
#' @noRd
prov_capture_args <- function(fun, env) {
  args <- list()
  if (is.null(fun)) return(args)

  for (a in setdiff(names(formals(fun)), c("x", "..."))) {
    value <- tryCatch(get(a, envir = env), error = function(e) NULL)
    if (is.null(value) || length(value) == 0) next
    args[[a]] <- if (is.atomic(value)) value else paste0("<", class(value)[1], " object>")
  }
  args
}

#' Is there a registry to write into?
#'
#' The registry only exists once par_set() has been called in this session.
#' Every recorder is a no-op before that, so a user who calls the internal
#' helpers outside a pipeline never triggers an error.
#' @noRd
prov_active <- function() {
  !is.null(.envar_prov$datasets)
}

#' Open a new dataset entry in the registry
#'
#' Called as the first statement of every dataset function (e.g. `chelsa()`).
#' The arguments of the calling function are captured automatically from its
#' formals, so the recorder does not need to know which arguments a given
#' dataset function takes. The pipeline object `x` and `...` are skipped: `x`
#' can be a large raster and is described separately by metadata().
#'
#' @param fun Name of the dataset function, e.g. `"chelsa"`.
#' @return Invisibly, `NULL`.
#' @noRd
prov_record_call <- function(fun) {
  if (!prov_active()) {
    # A dataset function called on a raster/data.frame from a previous session
    # (or on a saved object) still gets a registry, just without par_set()
    # settings.
    prov_reset(list())
  }

  # Collect the evaluated arguments of the calling dataset function. Arguments
  # that are missing and have no default (e.g. `vars` when the call fails
  # later) are simply skipped.
  caller <- tryCatch(sys.function(-1), error = function(e) NULL)
  args <- prov_capture_args(caller, parent.frame())

  entry <- list(
    fun = fun,
    time = Sys.time(),
    args = args,
    downloads = list(),
    native = list()
  )

  .envar_prov$datasets <- c(.envar_prov$datasets, list(entry))
  .envar_prov$current <- length(.envar_prov$datasets)
  invisible(NULL)
}

#' Bucket that the next recorded item belongs to
#'
#' Downloads and source rasters handled while a dataset function is running
#' belong to that dataset; those handled before any dataset function was called
#' (i.e. inside par_set(), when the study-area boundaries are fetched) belong to
#' the "boundary" bucket.
#' @noRd
prov_target <- function() {
  if (!prov_active()) return(NULL)
  if (is.null(.envar_prov$current)) "boundary" else .envar_prov$current
}

#' Record one download (URL, destination and time)
#'
#' Called by [download_file()] and [download_file_figshare()] for every file
#' that is retrieved, and also for files that are served from the download
#' cache, so that the report distinguishes the two.
#'
#' @param url URL that was requested.
#' @param dest_file Local file the download was stored in.
#' @param cached `TRUE` when an already downloaded copy was reused.
#' @return Invisibly, `NULL`.
#' @noRd
prov_add_download <- function(url, dest_file, cached = FALSE) {
  target <- prov_target()
  if (is.null(target)) return(invisible(NULL))

  record <- list(
    url = as.character(url)[1],
    file = basename(as.character(dest_file)[1]),
    time = Sys.time(),
    cached = isTRUE(cached)
  )

  if (identical(target, "boundary")) {
    .envar_prov$boundary$downloads <- c(.envar_prov$boundary$downloads, list(record))
  } else {
    entry <- .envar_prov$datasets[[target]]
    entry$downloads <- c(entry$downloads, list(record))
    .envar_prov$datasets[[target]] <- entry
  }
  invisible(NULL)
}

#' Record the native resolution and CRS of a source raster
#'
#' Called by [process_raster_layer()] and [process_points()] with the layer as
#' it was read from the downloaded file, i.e. before any aggregation,
#' resampling or reprojection. Identical records (the usual case, since all the
#' layers of one dataset share a grid) are stored once.
#'
#' @param layer A `SpatRaster` as read from the source file.
#' @return Invisibly, `NULL`.
#' @noRd
prov_add_native <- function(layer) {
  target <- prov_target()
  if (is.null(target)) return(invisible(NULL))

  record <- tryCatch({
    res <- terra::res(layer)
    lonlat <- isTRUE(terra::is.lonlat(layer))
    list(
      res_x = res[1],
      res_y = res[2],
      lonlat = lonlat,
      km = if (lonlat) res[2] * 111.32 else max(res) / 1000,
      crs = describe_crs(layer)
    )
  }, error = function(e) NULL)

  if (is.null(record)) return(invisible(NULL))

  if (identical(target, "boundary")) {
    existing <- .envar_prov$boundary$native
    if (!prov_has_native(existing, record)) {
      .envar_prov$boundary$native <- c(existing, list(record))
    }
  } else {
    entry <- .envar_prov$datasets[[target]]
    if (!prov_has_native(entry$native, record)) {
      entry$native <- c(entry$native, list(record))
      .envar_prov$datasets[[target]] <- entry
    }
  }
  invisible(NULL)
}

#' Has this native-resolution record already been stored?
#' @noRd
prov_has_native <- function(existing, record) {
  for (e in existing) {
    if (isTRUE(all.equal(e, record))) return(TRUE)
  }
  FALSE
}

#' Snapshot of the registry
#' @noRd
prov_get <- function() {
  list(
    started = .envar_prov$started,
    par = .envar_prov$par,
    datasets = if (is.null(.envar_prov$datasets)) list() else .envar_prov$datasets,
    boundary = if (is.null(.envar_prov$boundary)) list(downloads = list(), native = list()) else .envar_prov$boundary
  )
}

#' Human-readable description of a coordinate reference system
#'
#' Returns e.g. `"EPSG:3035 (ETRS89-extended / LAEA Europe)"`, falling back to
#' whatever is available when the CRS cannot be resolved.
#'
#' @param x A `SpatRaster`, an `sf`/`crs` object, or a CRS string such as
#'   `"EPSG:4326"`.
#' @return A single character string.
#' @noRd
describe_crs <- function(x) {
  if (is.null(x)) return(NA_character_)

  crs_obj <- tryCatch({
    if (inherits(x, "SpatRaster")) sf::st_crs(terra::crs(x)) else sf::st_crs(x)
  }, error = function(e) NULL)

  if (is.null(crs_obj) || is.na(crs_obj)) {
    return(if (is.character(x)) x else NA_character_)
  }

  name <- tryCatch(crs_obj$Name, error = function(e) NULL)
  code <- tryCatch({
    epsg <- crs_obj$epsg
    if (!is.null(epsg) && !is.na(epsg)) paste0("EPSG:", epsg) else NULL
  }, error = function(e) NULL)

  # ESRI codes (and any other authority) are not returned by $epsg, so fall
  # back to the identifier the user supplied when it looks like one.
  if (is.null(code) && is.character(x) && grepl("^(EPSG|ESRI):", x, ignore.case = TRUE)) {
    code <- x
  }

  if (!is.null(code) && !is.null(name) && nzchar(name)) {
    paste0(code, " (", name, ")")
  } else if (!is.null(code)) {
    code
  } else if (!is.null(name) && nzchar(name)) {
    name
  } else {
    NA_character_
  }
}
