#' @title Compute Aggregate of Matrix Values
#' @description Calculate the group by aggregate of a matrix
#' @param g An `igraph` object
#' @param m An adjacency matrix of numeric values. The number of column and rows must be identical. All rows and columns must be named. Ideally this is a `N*N` shortest path distance matrix.
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
#' # Calculate the shortest paths for all vertexes
#' my_quickest_paths <- distances(graph = BristolBathGraph,
#'                                weights = edge_attr(BristolBathGraph,"duration"))
#' 
#' # Calculate the maximum travel time between any two vertexes within a postcode area
#' # Returns a vector object of length 2
#' # The names represent postcode area
#' # The value represent the maximum travel duration of each postcode area
#' get_matrix_aggregate(g = BristolBathGraph, 
#'                      m = my_quickest_paths,
#'                      groups = area,
#'                      func = max)
#' 
#' # Optional arguments can be passed onto `func` using `...`.
#' # This will calculate the percentiles of the travel duration of each postcode area
#' # Returns a matrix object of 2 columns * 5 rows.
#' # The columns represent postcode areas
#' # The rows are names as '10%', '30%', '50%', '70%' and '90%'
#' # You may use `names=FALSE` to suppress row names (see documentation for `?quantile()`)
#' get_matrix_aggregate(g = BristolBathGraph, 
#'                      m = my_quickest_paths,
#'                      groups = area,
#'                      func = quantile,
#'                      probs = c(0.1, 0.3, 0.5, 0.7, 0.9))
#' 
#' get_matrix_aggregate(g = BristolBathGraph, 
#'                      m = my_quickest_paths,
#'                      groups = area,
#'                      func = quantile,
#'                      probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
#'                      names = FALSE)
#' @export
get_matrix_aggregate <- function(g,
                                 m,
                                 groups,
                                 func,
                                    ...) {
  
  if (base::nrow(m) != base::ncol(m)) {
    base::stop("Matrix m needs identical number of rows and columns")
  }
  
  if (!base::all(base::colnames(m) == base::rownames(m))){
    base::stop("Matrix m needs identical colnames and rownames")
  }
  
  base::sapply(base::as.character(base::unique(groups)), function(gp) {
    func(m
         [igraph::vertex_attr(g, "name"), igraph::vertex_attr(g, "name")]
         [groups == gp, groups == gp], 
         ...) },
    USE.NAMES = TRUE)
}