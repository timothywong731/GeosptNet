#' @title Compute Aggregate of Vertex Attributes
#' @description Calculate the group by aggregate of a specific vertex attribute
#' @param g An `igraph` object
#' @param attr Character value, indicating the vertex attribute to be aggregated
#' @param groups Vector of character or factor, which indicate the groups to be aggregated
#' @param func Aggregation function
#' @param ... Optional arguments to be passed onto the aggregation function
#' @return A vector or matrix object
#' @author Timothy Wong, \email{timothy.wong@@hotmail.co.uk}
#' @examples
#' # Use the postcode sector to derive the area
#' # Example: BS = Bristol; BA = Bath
#' library(igraph)
#' library(stringr)
#' area <- factor(str_extract(vertex_attr(BristolBathGraph, "name"),"^[A-Z]*"),
#'                labels = c(BS="Bristol", BA="Bath"))
#' 
#' # Calculate the aggregate of vertex attribute
#' # Returns a vector object of length 2
#' # The names represent postcode area
#' # The value represent total population of each postcode area
#' get_vertex_attr_aggregate(g = BristolBathGraph,
#'                           groups = area,
#'                           attr = "population", 
#'                           func = sum)
#' 
#' # Optional arguments can be passed onto `func` using `...`.
#' # This will calculate the percentiles of the population of each postcode area
#' # Returns a matrix object of 2 columns * 4 rows.
#' # The columns represent postcode areas
#' # The rows are names as '20%', '40%', '60%' and '80%'
#' # You may use `names=FALSE` to suppress row names (see documentation for `?quantile()`)
#' get_vertex_attr_aggregate(g = BristolBathGraph,
#'                           groups = area,
#'                           attr = "population", 
#'                           func = quantile,
#'                           probs = c(0.2, 0.4, 0.6, 0.8))
#' 
#' get_vertex_attr_aggregate(g = BristolBathGraph,
#'                           groups = area,
#'                           attr = "population", 
#'                           func = quantile,
#'                           probs = c(0.2, 0.4, 0.6, 0.8),
#'                           names = FALSE)
#' @export
get_vertex_attr_aggregate <- function(g, 
                                      attr, 
                                      groups,
                                      func,
                                      ...) {
  values <- igraph::vertex_attr(g, attr)
  by <- base::list(group = groups)
  
  if (base::length(by$group) != base::length(values)) {
    stop("Length of attributes and length of groups are not identical")
  }
  base::sapply(base::as.character(base::unique(by$group)),
               function(gp){func(values[gp==by$group],...)},
               USE.NAMES = TRUE)
  
}