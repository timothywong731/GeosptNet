#' Postcode Sectors in South-West England as an igraph Object
#'
#' An undirected graph (`igraph` object) containing Postcode Sectors of the Bristol (BS) and Bath (BA) area in South-West England of the United Kingdom. Vertexes represent postcode sectors while edges indicate transportation linkage between adjacent postcode sectors. 
#'
#' @format A `igraph` object with 245 vertexes and 650 edges.
#' 
#' Vertex attributes:
#' \describe{
#'   \item{name}{UK postcode sector (e.g. `BS1 1`, `BS6 5`)}
#'   \item{population}{Usual resident population, provided by UK Census 2011}
#'   \item{long}{Longitude of the postcode sector}
#'   \item{lat}{Latitude of the postcode sector}
#' }
#' 
#' Edge attributes:
#' \describe{
#'   \item{distance}{Road distance between the incident vertexes, in metres}
#'   \item{duration}{Travel time between the incident vertexes, in seconds}
#' }
#' @source 
#' \itemize{
#' \item {Usual resident population, UK Census 2011 (Table ID: KS101EW) \url{https://www.nomisweb.co.uk/census/2011/ks101ew}}
#' }
#' @examples
#' # Create a subgraph using the top 15 vertexes and plot it.
#' library(igraph)
#' library(sp)
#' set.seed(1000)
#' plot(induced_subgraph(BristolBathGraph, 1:15))
#' 
#' # Create a histogram showing the distribution of travel time
#' hist(edge_attr(BristolBathGraph, "duration"))
#' 
#' # Scatterplot showing distance and duration between postcode sectors 
#' plot(edge_attr(BristolBathGraph, "distance"),
#'      edge_attr(BristolBathGraph, "duration"),
#'      xlab = "Distance (m)",
#'      ylab = "Duration (seconds)")
"BristolBathGraph"


#' Boundaries of Postcode Sectors in South-West England as a SpatialPolygons Object
#'
#' A `SpatialPolygons` object containing boundaries of Postcode Sectors of the Bristol (BS) and Bath (BA) area in South-West England of the United Kingdom. 
#'
#' @format A `SpatialPolygons` object.
#' 
#' Each postcode sector has the following attributes:
#' \describe{
#'   \item{Name}{UK postcode sector (e.g. `BS1 1`, `BS6 5`)}
#'   \item{Description}{Empty string value}
#' }
#' @examples
#' library(sp)
#' plot(BristolBathPolygons)
"BristolBathPolygons"



#' Places in South-West England
#'
#' A `data frame` object containing several prominent towns and cities in South-West England.
#'
#' @format A `data frame` object.
#' \describe{
#'   \item{place}{City or town}
#'   \item{lat}{Latitude}
#'   \item{long}{Longitude}
#' }
#' @examples
#' BristolBathPlaces
"BristolBathPlaces"