# R/data.R

#' Alpine region
#'
#' A spatial dataset (class "sf", "data.frame") containing the borders of the European Alps region,
#' retrieved from the Alpine Convention website (https://www.alpconv.org/fr/page-daccueil/).
#' Used for examples and testing within the envar package.
#'
#' @format An sf object :
#' \describe{	
#'   \item{name}{European Alps}
#'   \item{geometry}{sfc_MULTIPOLYGON geometry}
#' }
#' @source Alpine Convention
"Alps"

#' Europe
#'
#' Europe shapefile from the GADM, the Global Administrative Areas database.
#' 
#' @format An sf object :
#' \describe{	
#'   \item{name}{Europe}
#'   \item{geometry}{sfc_MULTIPOLYGON geometry}
#' }
#' @source GADM
"Europe"

#' Apollo butterfly occurrences
#'
#' A "data.frame" containing the occurrence data of Apollo butterfly (Parnassius apollo),
#' downloaded from the Global Biodiversity Information Facility website (www.gbif.org), on December 10, 2025.
#' Used for examples and testing within the envar package.
#'
#' @format A data.frame object :
#' \describe{	
#'   \item{X}{longitude in WGS84}
#'   \item{Y}{latitude in WGS84}
#' }
#' @source GBIF
"Apollo"
