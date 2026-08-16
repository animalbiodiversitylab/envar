# R/metadata.R

#' Store a Provenance Summary of an `envar` Pipeline
#'
#' `metadata()` is an optional function for the `envar` package workflow. Added
#' at the end of a pipeline, it collects everything the pipeline did and writes
#' it to disk as a reproducibility record: the data sources that were contacted
#' (source URLs and the date each file was downloaded), the variables that were
#' produced, the temporal period they refer to, the native resolution of each
#' source dataset, the resolution and coordinate reference system of the output,
#' and the processing settings that were used (those given to [par_set()] as
#' well as the arguments of every dataset function that was called).
#'
#' @param x The object flowing through the pipeline: a `SpatRaster`, a
#'   `data.frame` of extracted values, or a list such as the one returned by
#'   [extr_check()] or [corr_check()].
#'
#' @details
#' The information is gathered while the pipeline runs. [par_set()] clears the
#' record and stores the study-area settings, each dataset function registers
#' its own call, and every download and every source raster is registered as it
#' is handled. `metadata()` therefore describes the pipeline that started with
#' the most recent call to [par_set()] in the current R session; the output
#' object itself (variables, resolution, CRS, extent) is described directly from
#' `x`.
#'
#' Two files are written: a readable report (`envar_metadata.txt`) and the same
#' information as a table with one row per dataset (`envar_metadata.csv`). As in
#' [corr_check()], an interactive R session asks at the console for the
#' directory to store them in every time the function is called, with an empty
#' answer meaning the working directory. When a pipeline contains both
#' `corr_check()` and `metadata()` the question is therefore asked twice, once
#' for each function. In non-interactive sessions (e.g. scripts or `R CMD
#' check`) a temporary directory is used and no prompt is shown.
#'
#' @return `x`, so that the pipeline object is not altered, with the summary
#'   attached:
#'   \itemize{
#'     \item for a list input, as the elements `metadata` (the summary as a
#'       `data.frame`) and `metadata_path` (the paths of the two files written);
#'     \item for a `SpatRaster` or `data.frame` input, as the attributes
#'       `"envar_metadata"` and `"metadata_path"`, retrievable with
#'       `attr(x, "envar_metadata")`.
#'   }
#'
#' @examples
#' # Called outside a pipeline, metadata() still describes the object it is
#' # given (variables, resolution, CRS, extent) and reports that no download was
#' # recorded. This runs offline on the example raster bundled with the package:
#' switzerland <- terra::rast(
#'   system.file("extdata", "switzerland.tif", package = "envar")
#' )
#' m <- metadata(switzerland)
#' attr(m, "metadata_path")
#'
#' \donttest{
#' # Store the provenance of a pipeline
#' processed <- par_set(country = "Italy", crs = 3035, buffer = 10) %>%
#'   melc(vars = c("ice")) %>%
#'   chelsa(vars = c("pr"), months = 12, year = 2015) %>%
#'   metadata()
#'
#' # The summary is also returned with the object
#' attr(processed, "envar_metadata")
#'
#' # Together with corr_check(): the directory is asked twice, once per function
#' checked <- par_set(country = "Italy") %>%
#'   chelsa(vars = c("bio1", "bio12")) %>%
#'   corr_check() %>%
#'   metadata()
#'
#' checked$metadata
#' }
#' @export
metadata <- function(x) {

  if (!inherits(x, "SpatRaster") && !inherits(x, "data.frame") && !is.list(x)) {
    cli::cli_abort(
      "Input must be a {.cls SpatRaster}, a {.cls data.frame}, or a list produced by an {.pkg envar} pipeline."
    )
  }

  # -------------------------------------------------------------------------
  # Locate the object the pipeline actually produced
  # -------------------------------------------------------------------------
  is_list_input <- is.list(x) && !inherits(x, "SpatRaster") && !inherits(x, "data.frame")

  if (is_list_input) {
    data_element <- if ("data" %in% names(x)) {
      x$data
    } else if ("extracted_df" %in% names(x)) {
      x$extracted_df
    } else if (inherits(x[[1]], "SpatRaster") || inherits(x[[1]], "data.frame")) {
      x[[1]]
    } else {
      cli::cli_abort("List input must contain a {.field data} or {.field extracted_df} element.")
    }
  } else {
    data_element <- x
  }

  prov <- prov_get()

  if (length(prov$datasets) == 0) {
    cli::cli_alert_warning(c(
      "No {.pkg envar} download was recorded in this session, so the source and download information is empty. ",
      "Run {.fn metadata} in the same session as the pipeline it should describe."
    ))
  }

  # -------------------------------------------------------------------------
  # Describe the pipeline output
  # -------------------------------------------------------------------------
  output_info <- describe_output(data_element)

  # -------------------------------------------------------------------------
  # Build the summary table (one row per dataset function called)
  # -------------------------------------------------------------------------
  summary_df <- build_metadata_table(prov, output_info)

  # -------------------------------------------------------------------------
  # Ask where to store the files, exactly as corr_check() does, and write them
  # -------------------------------------------------------------------------
  out_dir <- envar_output_dir("metadata")

  txt_path <- file.path(out_dir, "envar_metadata.txt")
  csv_path <- file.path(out_dir, "envar_metadata.csv")

  report <- format_metadata_report(prov, output_info, summary_df)

  tryCatch({
    writeLines(report, txt_path, useBytes = TRUE)
    cli::cli_alert_info("Metadata summary saved to {.file {txt_path}}.")
  }, error = function(e) {
    cli::cli_alert_warning("Could not save the metadata summary: {e$message}")
    txt_path <<- NA_character_
  })

  tryCatch({
    utils::write.csv(summary_df, csv_path, row.names = FALSE)
    cli::cli_alert_info("Metadata table saved to {.file {csv_path}}.")
  }, error = function(e) {
    cli::cli_alert_warning("Could not save the metadata table: {e$message}")
    csv_path <<- NA_character_
  })

  cli::cli_alert_success("Metadata summary completed.")

  paths <- c(summary = txt_path, table = csv_path)

  # -------------------------------------------------------------------------
  # Return the pipeline object with the summary attached
  # -------------------------------------------------------------------------
  if (is_list_input) {
    x$metadata <- summary_df
    x$metadata_path <- paths
  } else {
    attr(x, "envar_metadata") <- summary_df
    attr(x, "metadata_path") <- paths
  }

  x
}


#' Describe the object produced by the pipeline
#'
#' Extracts the variable names, the output resolution, the CRS and the extent
#' from the final `SpatRaster` or `data.frame`.
#' @noRd
describe_output <- function(data_element) {

  if (inherits(data_element, "SpatRaster")) {
    res <- terra::res(data_element)
    crs_txt <- describe_crs(data_element)
    lonlat <- isTRUE(terra::is.lonlat(data_element))
    unit <- if (lonlat) "degrees" else "map units (metres for most projected CRS)"
    ext <- as.vector(terra::ext(data_element))

    return(list(
      type = paste0("SpatRaster stack (", terra::nlyr(data_element), " layer(s))"),
      variables = names(data_element),
      resolution = paste0(
        format(res[1], digits = 8), " x ", format(res[2], digits = 8), " ", unit,
        if (lonlat) paste0(" (~", round(res[2] * 111.32, 3), " km)") else
          paste0(" (~", round(max(res) / 1000, 3), " km)")
      ),
      crs = crs_txt,
      extent = paste0(
        "xmin ", format(ext[1], digits = 10), ", xmax ", format(ext[2], digits = 10),
        ", ymin ", format(ext[3], digits = 10), ", ymax ", format(ext[4], digits = 10)
      ),
      dimensions = paste0(
        terra::nrow(data_element), " rows x ", terra::ncol(data_element),
        " columns x ", terra::nlyr(data_element), " layers"
      )
    ))
  }

  # data.frame of values extracted at points
  non_vars <- c("ID", "X", "Y", "x", "y", "id", "strict", "combinatorial")
  variables <- names(data_element)[!tolower(names(data_element)) %in% tolower(non_vars)]
  crs_txt <- describe_crs(attr(data_element, "envar_crs"))
  if (is.na(crs_txt)) crs_txt <- describe_crs(prov_get()$par$crs)

  list(
    type = paste0("data.frame of extracted values (", nrow(data_element), " point(s))"),
    variables = variables,
    resolution = "point extraction: values are read at the point locations, no output grid",
    crs = crs_txt,
    extent = if (all(c("X", "Y") %in% names(data_element))) {
      paste0(
        "xmin ", format(min(data_element$X, na.rm = TRUE), digits = 10),
        ", xmax ", format(max(data_element$X, na.rm = TRUE), digits = 10),
        ", ymin ", format(min(data_element$Y, na.rm = TRUE), digits = 10),
        ", ymax ", format(max(data_element$Y, na.rm = TRUE), digits = 10)
      )
    } else NA_character_,
    dimensions = paste0(nrow(data_element), " rows x ", ncol(data_element), " columns")
  )
}


#' Reference information for the datasets served by envar
#'
#' Source name, citation and native temporal coverage of each dataset function.
#' The temporal coverage is only given when it is a fixed property of the
#' dataset; for the datasets whose period is chosen by the user (e.g. `chelsa()`
#' or `worldclim()`) it is `NA` and the period is taken from the arguments of
#' the call instead.
#' @noRd
envar_dataset_reference <- function(fun) {

  ref <- list(
    accessibility = list(
      source = "Global Accessibility Indicators (travel time to cities and ports)",
      reference = "Nelson A, Weiss DJ, van Etten J et al. (2019). A suite of global accessibility indicators. Scientific Data 6, 266. doi:10.1038/s41597-019-0265-5",
      coverage = "2015"
    ),
    aridity = list(
      source = "Global Aridity Index and Potential Evapotranspiration (ET0) Database v3",
      reference = "Zomer RJ, Xu J, Trabucco A (2022). Version 3 of the Global Aridity Index and Potential Evapotranspiration Database. Scientific Data 9, 409. doi:10.1038/s41597-022-01493-1",
      coverage = "1970-2000 climatology"
    ),
    biooracle = list(
      source = "Bio-ORACLE v3.0 marine layers",
      reference = "Assis J, Fernandez Bejarano SJ, Salazar VW et al. (2024). Bio-ORACLE v3.0. Global Ecology and Biogeography 33, e13813. doi:10.1111/geb.13813",
      coverage = NA_character_
    ),
    chelsa = list(
      source = "CHELSA - Climatologies at High Resolution for the Earth's Land Surface Areas",
      reference = "Karger DN, Conrad O, Bohner J et al. (2017). Climatologies at high resolution for the earth's land surface areas. Scientific Data 4, 170122. doi:10.1038/sdata.2017.122; BIOCLIM+: Brun P, Zimmermann NE, Hari C, Pellissier L, Karger DN (2022). Earth System Science Data 14, 5573-5603. doi:10.5194/essd-14-5573-2022",
      coverage = NA_character_
    ),
    climatezones = list(
      source = "High-resolution (1 km) Koppen-Geiger climate classification maps",
      reference = "Beck HE, McVicar TR, Vergopolan N et al. (2023). High-resolution (1 km) Koppen-Geiger maps for 1901-2099 based on constrained CMIP6 projections. Scientific Data 10, 724. doi:10.1038/s41597-023-02549-6",
      coverage = NA_character_
    ),
    cloudcover = list(
      source = "EarthEnv Global 1 km Cloud Cover",
      reference = "Wilson AM, Jetz W (2016). Remotely sensed high-resolution global cloud dynamics for predicting ecosystem and biodiversity distributions. PLoS Biology 14(3), e1002415. doi:10.1371/journal.pbio.1002415",
      coverage = NA_character_
    ),
    earthenvlandcover = list(
      source = "EarthEnv Consensus Land Cover",
      reference = "Tuanmu MN, Jetz W (2014). A global 1-km consensus land-cover product for biodiversity and ecosystem modeling. Global Ecology and Biogeography 23, 1031-1045. doi:10.1111/geb.12182",
      coverage = NA_character_
    ),
    freshwater = list(
      source = "EarthEnv near-global freshwater-specific environmental variables",
      reference = "Domisch S, Amatulli G, Jetz W (2015). Near-global freshwater-specific environmental variables for biodiversity analyses in 1 km resolution. Scientific Data 2, 150073. doi:10.1038/sdata.2015.73",
      coverage = "static layers (2015 release)"
    ),
    gcamlandcover = list(
      source = "Global future land use / land cover 2020-2100 at 1 km",
      reference = "Zhang T, Cheng C, Wu X (2023). Mapping the spatial heterogeneity of global land use and land cover from 2020 to 2100 at a 1 km resolution. Scientific Data 10, 748. doi:10.1038/s41597-023-02637-7",
      coverage = NA_character_
    ),
    gdppast = list(
      source = "Global 1 km gridded revised real GDP and electricity consumption",
      reference = "Chen J, Gao M, Cheng S et al. (2022). Global 1 km x 1 km gridded revised real gross domestic product and electricity consumption during 1992-2019 based on calibrated nighttime light data. Scientific Data 9, 202. doi:10.1038/s41597-022-01322-5",
      coverage = "1992-2019"
    ),
    geososlandcover = list(
      source = "GEOSOS global land-use and land-cover change product 2010-2100",
      reference = "Li X, Chen G, Liu X et al. (2017). A new global land-use and land-cover change product at a 1-km resolution for 2010 to 2100 based on human-environment interactions. Annals of the American Association of Geographers 107(5), 1040-1059. doi:10.1080/24694452.2017.1303357",
      coverage = NA_character_
    ),
    habitat = list(
      source = "IUCN global habitat classification fractions",
      reference = "Jung M, Dahal PR, Butchart SHM et al. (2020). A global map of terrestrial habitat types. Scientific Data 7, 256. doi:10.1038/s41597-020-00599-8",
      coverage = "2015"
    ),
    heterogeneity = list(
      source = "EarthEnv global habitat heterogeneity",
      reference = "Tuanmu MN, Jetz W (2015). A global, remote sensing-based characterization of terrestrial habitat heterogeneity for biodiversity and ecosystem modelling. Global Ecology and Biogeography 24, 1329-1339. doi:10.1111/geb.12365",
      coverage = NA_character_
    ),
    hybridlandcover = list(
      source = "Hybrid global annual 1 km IGBP land cover maps",
      reference = "Luo Y, Zhu Z, Zhao W et al. (2024). Hybrid Global Annual 1-km IGBP Land Cover Maps for the Period 2000-2020. Journal of Remote Sensing 4, 0122. doi:10.34133/remotesensing.0122",
      coverage = NA_character_
    ),
    melc = list(
      source = "MELC - MacroEcological Land Cover (1 km, from ESA very high resolution imagery)",
      reference = "Lo Parrino E, Simoncini A, Ficetola GF, Falaschi M (2025). Global 1 km land cover for macroecological modelling from very high resolution imagery. Figshare. doi:10.6084/m9.figshare.30665069",
      coverage = NA_character_
    ),
    pftlandcover = list(
      source = "Global 7-land-type land-cover projections based on plant functional types",
      reference = "Chen G, Li X, Liu X (2022). Global land projection based on plant functional types with a 1-km resolution under socio-climatic scenarios. Scientific Data 9, 125. doi:10.1038/s41597-022-01208-6",
      coverage = NA_character_
    ),
    population = list(
      source = "Global 1 km population projections under the Shared Socioeconomic Pathways",
      reference = "Wang X, Meng X, Long Y (2022). Projecting 1 km-grid population distributions from 2020 to 2100 globally under shared socioeconomic pathways. Scientific Data 9, 563. doi:10.1038/s41597-022-01675-x",
      coverage = NA_character_
    ),
    protection = list(
      source = "World Database on Protected Areas (WDPA), rasterised by IUCN management category",
      reference = "Protected Planet (2025). World Database on Protected Areas (WDPA). UNEP-WCMC. https://www.protectedplanet.net/en",
      coverage = "WDPA, 2025 release"
    ),
    roads = list(
      source = "Global road density (~1 km) derived from OpenStreetMap through GeoFabrik",
      reference = "OpenStreetMap contributors, accessed through GeoFabrik (https://www.geofabrik.de/). Hosted on Figshare under embargo; check with the data authors before redistributing.",
      coverage = "OpenStreetMap database as of 30 January 2026"
    ),
    soil = list(
      source = "Harmonized World Soil Database v2.0 (HWSD v2.0)",
      reference = "FAO, IIASA (2023). Harmonized World Soil Database v2.0. FAO, Rome and IIASA, Laxenburg. https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/",
      coverage = "static (soil types, 2023 release)"
    ),
    soilclimate = list(
      source = "Global soil temperature and soil bioclimatic layers",
      reference = "Lembrechts JJ et al. (2022). Global maps of soil temperature. Global Change Biology 28, 3110-3144. doi:10.1111/gcb.16060",
      coverage = NA_character_
    ),
    spectre = list(
      source = "SPECTRE - Spatially Explicit ECosysTem ThREats",
      reference = "Branco VV, Capinha C, Rocha J, Correia L, Cardoso P (2024). SPECTRE: standardized global spatial data on terrestrial SPecies and ECosystems ThREats. Global Ecology and Biogeography 34, e13949. doi:10.1111/geb.13949",
      coverage = NA_character_
    ),
    topography = list(
      source = "EarthEnv global cross-scale topographic variables",
      reference = "Amatulli G, Domisch S, Tuanmu MN et al. (2018). A suite of global, cross-scale topographic variables for environmental and biodiversity modeling. Scientific Data 5, 180040. doi:10.1038/sdata.2018.40",
      coverage = "static (elevation-derived)"
    ),
    worldclim = list(
      source = "WorldClim 2.1 (historical) and downscaled CMIP6 projections (future)",
      reference = "Fick SE, Hijmans RJ (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37, 4302-4315. doi:10.1002/joc.5086",
      coverage = NA_character_
    )
  )

  if (!is.null(ref[[fun]])) {
    return(ref[[fun]])
  }

  list(source = fun, reference = NA_character_, coverage = NA_character_)
}


#' Arguments that describe the temporal period of a dataset call
#' @noRd
temporal_args <- c("years", "year", "months", "month", "period", "periods",
                   "time", "times", "cruts_years", "decade", "decades",
                   "ssp", "rcp", "gcm", "scenario", "scenarios")


#' Collapse a vector of argument values into a short readable string
#' @noRd
format_arg_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  # format() pads a character vector to a common width, which would show up in
  # the report as trailing blanks, so only numbers are formatted.
  if (is.numeric(value)) {
    return(paste(format(value, trim = TRUE, scientific = FALSE), collapse = ", "))
  }
  paste(as.character(value), collapse = ", ")
}


#' Temporal period of one recorded dataset call
#' @noRd
entry_temporal <- function(entry) {
  used <- intersect(names(entry$args), temporal_args)
  # Scenario arguments only qualify the period; they are not a period in
  # themselves, so they are reported only alongside a year/period argument.
  period_args <- setdiff(used, c("ssp", "rcp", "gcm", "scenario", "scenarios"))

  parts <- character(0)
  for (a in used) {
    parts <- c(parts, paste0(a, " = ", format_arg_value(entry$args[[a]])))
  }

  coverage <- envar_dataset_reference(entry$fun)$coverage

  if (length(period_args) > 0) {
    txt <- paste(parts, collapse = "; ")
    if (!is.na(coverage)) txt <- paste0(txt, " (dataset coverage: ", coverage, ")")
    return(txt)
  }

  if (!is.na(coverage)) {
    if (length(parts) > 0) {
      return(paste0(coverage, " (", paste(parts, collapse = "; "), ")"))
    }
    return(coverage)
  }

  if (length(parts) > 0) return(paste(parts, collapse = "; "))

  "not set in the call; the dataset default was used (see the function documentation)"
}


#' Native resolution of one recorded dataset call
#' @noRd
entry_native_res <- function(entry) {
  if (length(entry$native) == 0) return(NA_character_)

  txt <- vapply(entry$native, function(n) {
    unit <- if (isTRUE(n$lonlat)) "degrees" else "map units"
    paste0(
      format(n$res_x, digits = 8), " x ", format(n$res_y, digits = 8), " ", unit,
      " (~", round(n$km, 3), " km) in ", n$crs
    )
  }, character(1))

  paste(unique(txt), collapse = " | ")
}


#' Source URLs of one recorded dataset call
#' @noRd
entry_urls <- function(entry) {
  if (length(entry$downloads) == 0) return(character(0))
  unique(vapply(entry$downloads, function(d) d$url, character(1)))
}


#' Download dates of one recorded dataset call
#' @noRd
entry_dates <- function(entry) {
  if (length(entry$downloads) == 0) {
    return("no file was retrieved (nothing was downloaded and no cached copy was used)")
  }

  times <- as.POSIXct(vapply(entry$downloads, function(d) as.numeric(d$time), numeric(1)),
                      origin = "1970-01-01", tz = Sys.timezone())
  cached <- vapply(entry$downloads, function(d) isTRUE(d$cached), logical(1))

  stamp <- if (length(times) == 1) {
    format(times, "%Y-%m-%d %H:%M:%S")
  } else {
    paste0(format(min(times), "%Y-%m-%d %H:%M:%S"), " to ",
           format(max(times), "%Y-%m-%d %H:%M:%S"))
  }

  paste0(
    length(times), " file(s): ", stamp,
    if (any(cached)) paste0(" (", sum(cached), " reused from the download cache)") else ""
  )
}


#' Processing settings applied to one recorded dataset call
#' @noRd
entry_settings <- function(entry) {
  # `vars` is reported on its own, and the temporal arguments in the temporal
  # period, so neither is repeated here.
  other <- setdiff(names(entry$args), c(temporal_args, "vars"))
  if (length(other) == 0) return(NA_character_)
  paste(vapply(other, function(a) {
    paste0(a, " = ", format_arg_value(entry$args[[a]]))
  }, character(1)), collapse = "; ")
}


#' Variables requested in one recorded dataset call
#' @noRd
entry_vars <- function(entry) {
  if (is.null(entry$args$vars)) return(NA_character_)
  format_arg_value(entry$args$vars)
}


#' Settings given to par_set(), as a named character vector
#' @noRd
par_settings_txt <- function(par) {
  if (length(par) == 0) return(character(0))

  # The study area can be defined by any of several mutually exclusive
  # arguments; only the ones actually supplied are reported.
  extent_args <- c("shape", "country", "continent", "ecoregion", "biome", "realm",
                   "zooregion", "zoorealm", "mountain_region", "mountain_region_cmec",
                   "glacier_region_19", "glacier_region_20", "freshwater_ecoregion",
                   "marine_ecoregion", "marine_realm", "marine_province",
                   "pelagic_province", "pelagic_biome", "pelagic_realm", "pointsdf")

  used_extent <- character(0)
  for (a in intersect(extent_args, names(par))) {
    if (!is.null(par[[a]])) {
      value <- if (is.character(par[[a]]) && length(par[[a]]) <= 5) {
        format_arg_value(par[[a]])
      } else {
        paste0("<", class(par[[a]])[1], ">")
      }
      used_extent <- c(used_extent, paste0(a, " = ", value))
    }
  }
  if (length(used_extent) == 0) {
    used_extent <- "global extent (no study area argument was supplied)"
  }

  out <- c(
    "Study area" = paste(used_extent, collapse = "; "),
    "Buffer" = paste0(format_arg_value(par$buffer), " km"),
    "Alpha hull" = format_arg_value(par$alpha_hull),
    "Target CRS" = describe_crs(par$crs),
    "Resolution setting" = paste0(
      "res = ", format_arg_value(par$res),
      " (multiplier of the 30 arc-second base grid, ~", format_arg_value(par$res), " km)"
    ),
    "Boundary scale" = format_arg_value(par$scale),
    "Common NA mask (set_na)" = format_arg_value(par$set_na),
    "Clipped to land (land)" = format_arg_value(par$land),
    "Download cache" = format_arg_value(par$cache),
    "Output written to (path)" = if (is.null(par$path)) "not set" else format_arg_value(par$path),
    "Resampling method" = paste0(
      getOption("envar.resample_method", "auto"),
      " (auto: bilinear for continuous layers, nearest neighbour for categorical ones)"
    )
  )

  out[!is.na(out)]
}


#' Build the one-row-per-dataset metadata table
#' @noRd
build_metadata_table <- function(prov, output_info) {

  if (length(prov$datasets) == 0) {
    return(data.frame(
      dataset = character(0), source = character(0), reference = character(0),
      variables_requested = character(0), temporal_period = character(0),
      native_resolution = character(0), source_urls = character(0),
      download_dates = character(0), output_variables = character(0),
      output_resolution = character(0), output_crs = character(0),
      processing_settings = character(0), stringsAsFactors = FALSE
    ))
  }

  par_txt <- par_settings_txt(prov$par)
  par_flat <- if (length(par_txt) == 0) NA_character_ else {
    paste(paste0(names(par_txt), ": ", par_txt), collapse = "; ")
  }

  rows <- lapply(prov$datasets, function(entry) {
    ref <- envar_dataset_reference(entry$fun)
    urls <- entry_urls(entry)
    call_settings <- entry_settings(entry)

    data.frame(
      dataset = entry$fun,
      source = ref$source,
      reference = ifelse(is.na(ref$reference), "", ref$reference),
      variables_requested = ifelse(is.na(entry_vars(entry)), "", entry_vars(entry)),
      temporal_period = entry_temporal(entry),
      native_resolution = ifelse(is.na(entry_native_res(entry)), "", entry_native_res(entry)),
      source_urls = if (length(urls) == 0) "" else paste(urls, collapse = " | "),
      download_dates = entry_dates(entry),
      output_variables = paste(output_info$variables, collapse = ", "),
      output_resolution = output_info$resolution,
      output_crs = ifelse(is.na(output_info$crs), "", output_info$crs),
      processing_settings = paste(
        c(if (!is.na(par_flat)) paste0("par_set -> ", par_flat),
          if (!is.na(call_settings)) paste0(entry$fun, " -> ", call_settings)),
        collapse = " || "
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


#' Format the readable metadata report
#' @noRd
format_metadata_report <- function(prov, output_info, summary_df) {

  pad <- function(label) formatC(label, width = 26, flag = "-")
  line <- function(label, value) paste0("  ", pad(label), ": ", value)
  rule <- function(char = "-") paste(rep(char, 78), collapse = "")

  txt <- c(
    rule("="),
    "envar metadata summary",
    rule("="),
    line("Generated", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    line("Pipeline started", if (is.null(prov$started)) "unknown" else
      format(prov$started, "%Y-%m-%d %H:%M:%S %Z")),
    line("envar version", as.character(utils::packageVersion("envar"))),
    line("R version", paste(R.version$major, R.version$minor, sep = ".")),
    line("terra version", as.character(utils::packageVersion("terra"))),
    ""
  )

  # -- Study area and processing settings ----------------------------------
  txt <- c(txt, "STUDY AREA AND PROCESSING SETTINGS", rule())
  par_txt <- par_settings_txt(prov$par)
  if (length(par_txt) == 0) {
    txt <- c(txt, "  No par_set() call was recorded in this session.", "")
  } else {
    txt <- c(txt, unname(mapply(line, names(par_txt), par_txt)), "")
  }

  # -- Output ---------------------------------------------------------------
  txt <- c(
    txt,
    "OUTPUT", rule(),
    line("Type", output_info$type),
    line("Output resolution", output_info$resolution),
    line("Output CRS", ifelse(is.na(output_info$crs), "unknown", output_info$crs)),
    line("Extent", ifelse(is.na(output_info$extent), "unknown", output_info$extent)),
    line("Dimensions", output_info$dimensions),
    line(paste0("Variables (", length(output_info$variables), ")"),
         paste(output_info$variables, collapse = ", ")),
    ""
  )

  # -- Data sources ---------------------------------------------------------
  txt <- c(txt, "DATA SOURCES", rule())

  if (length(prov$datasets) == 0) {
    txt <- c(txt, "  No envar dataset function was recorded in this session.", "")
  } else {
    for (i in seq_along(prov$datasets)) {
      entry <- prov$datasets[[i]]
      ref <- envar_dataset_reference(entry$fun)
      urls <- entry_urls(entry)
      call_settings <- entry_settings(entry)

      txt <- c(
        txt,
        paste0("  [", i, "] ", entry$fun, "()  -  called on ",
               format(entry$time, "%Y-%m-%d %H:%M:%S")),
        line("Source", ref$source),
        if (!is.na(ref$reference)) line("Reference", ref$reference),
        line("Variables requested",
             ifelse(is.na(entry_vars(entry)), "dataset default", entry_vars(entry))),
        line("Temporal period", entry_temporal(entry)),
        line("Native resolution",
             ifelse(is.na(entry_native_res(entry)),
                    "not recorded (no source raster was processed)",
                    entry_native_res(entry))),
        line("Files retrieved", entry_dates(entry)),
        if (!is.na(call_settings)) line("Call settings", call_settings),
        if (length(urls) > 0) c("    Source URLs:", paste0("      - ", urls)),
        ""
      )
    }
  }

  # -- Study-area boundaries ------------------------------------------------
  boundary_urls <- if (length(prov$boundary$downloads) == 0) character(0) else {
    unique(vapply(prov$boundary$downloads, function(d) d$url, character(1)))
  }
  if (length(boundary_urls) > 0) {
    boundary_times <- as.POSIXct(
      vapply(prov$boundary$downloads, function(d) as.numeric(d$time), numeric(1)),
      origin = "1970-01-01", tz = Sys.timezone()
    )
    txt <- c(
      txt,
      "STUDY-AREA BOUNDARY SOURCES", rule(),
      line("Files retrieved", paste0(
        length(boundary_times), " file(s): ",
        format(min(boundary_times), "%Y-%m-%d %H:%M:%S"), " to ",
        format(max(boundary_times), "%Y-%m-%d %H:%M:%S")
      )),
      "    Source URLs:",
      paste0("      - ", boundary_urls),
      ""
    )
  }

  txt <- c(
    txt,
    rule("="),
    "Cite the original data sources listed above when publishing results, and",
    "cite the package itself as shown by citation('envar').",
    rule("=")
  )

  txt
}
