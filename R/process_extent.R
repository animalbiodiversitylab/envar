# R/process_extent.R
#' Process extent input
#' @noRd
process_extent <- function(shape = NULL, country = NULL, continent = NULL, buffer = 0) {
  
  extent_info <- list(type = NULL, bbox = NULL, mask = NULL, points = NULL)
  
  # ---- Input validation & priority resolution ----
  input_sources <- list(
    shape = !is.null(shape),
    country = !is.null(country),
    continent = !is.null(continent)
  )
  
  active_sources <- names(Filter(identity, input_sources))
  
  if (length(active_sources) > 1) {
    cli::cli_alert_warning("Hai specificato più sorgenti di extent: {paste(active_sources, collapse = ', ')}. Verrà usata la seguente priorità: shape > country > continent.")
  }
  
  # ---- 1. SHAPE (highest priority) ----
  if (!is.null(shape)) {
    if (!inherits(shape, "sf") && !inherits(shape, "sfc")) {
      cli::cli_abort("Il parametro `shape` deve essere un oggetto `sf` o `sfc`.")
    }
    
    extent_info$type <- "polygon"
    extent_info$mask <- shape
    
    if (buffer > 0) {
      extent_buffered <- sf::st_buffer(shape, dist = buffer * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      extent_info$bbox <- sf::st_bbox(shape)
    }
    
    return(extent_info)
  }
  
  # ---- 2. COUNTRY (second priority) ----
  if (!is.null(country)) {
    extent_info$type <- "admin"
    
    tryCatch({
      extent_info$mask <- rnaturalearth::ne_countries(
        country = country,
        returnclass = "sf",
        scale = "medium"
      )
    }, error = function(e) {
      cli::cli_abort("Paese non trovato: {.val {country}}.")
    })
    
    if (buffer > 0) {
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ---- 3. CONTINENT (lowest priority) ----
  if (!is.null(continent)) {
    extent_info$type <- "admin"
    
    tryCatch({
      extent_info$mask <- rnaturalearth::ne_countries(
        continent = continent,
        returnclass = "sf",
        scale = "medium"
      )
    }, error = function(e) {
      cli::cli_abort("Continente non trovato: {.val {continent}}.")
    })
    
    if (buffer > 0) {
      extent_buffered <- sf::st_buffer(extent_info$mask, dist = buffer * 1000)
      extent_info$bbox <- sf::st_bbox(extent_buffered)
    } else {
      extent_info$bbox <- sf::st_bbox(extent_info$mask)
    }
    
    return(extent_info)
  }
  
  # ---- 4. Missing input fallback ----
  cli::cli_abort("Devi specificare almeno uno tra: `shape`, `country`, `continent`.")
}
