#' @title Detect Communities in a Graph
#' @description Runs community detection algorithm to identify communities in a graph.
#' @param z A `data frame` object containing column `name` which contains all vertex names in graph `g`, and at least  one named column of characters indicating zones names.
#' @param g An `igraph` object
#' @param at_level Character value indicating the level to run community detection algorithm at. The level must exist as a column of character vector in the data frame `z`.
#' @param assign_level Character value indicating the level to assign the community detection output. 
#' @param edge_attribute Character value which indicates the edge attribute to be used for community detection algorithm.
#' @param allow_exit_zone Logical value indicating whether travel routes outside zonal boundary are allowed. Default value is `FALSE`. For example, if a zone is U-shaped then direct traveling between the two tips is allowed if this option is set to `TRUE`. The cost of the route is determined by the adjacency matrix `m`. Enabling this option may lead to longer compute time.
#' @param m Optional parameter. An adjacency matrix of numeric values. The number of column and rows must be identical. All rows and columns must be named. Ideally this is a `N*N` shortest path distance matrix. This argument is only required when `allow_exit_zone` is set to `TRUE`.
#' @param within_zones Optional parameter. Vector of characters indicating names of zones to detect community within. Default value is `NULL` which will process all zones ar the level specified in `at_level`.
#' @param max_non_adjacent_path_length Optional parameter. Integer value indicating the longest path which may connect two non-adjacent vertexes. 
#' Please note that long path length will lead to exclaves.
#' @param penalty A function which takes one numeric vector as input. Implements appropriate transformation and penalty. The output is used to calculate graph modularity in the algorithm. Please implement your own penalty function depending on what `edge_attribute` is being used.
#' 
#' @return A `data frame` object
#' @author Timothy Wong, \email{timothy.wong@@hotmail.co.uk}
#' @examples
#' library(igraph)
#' # Create a data frame object with vertex name
#' # `l1` is a character column indicating the top level zone in the hierarchy
#' z <- data.frame(name = igraph::vertex_attr(BristolBathGraph, "name"),
#'                 l1 = "SW England")
#' # This will split the top level zone into smaller zones at `l2`
#' z <-  detect_communities(z = z,
#'                          g = BristolBathGraph,
#'                          at_level = "l1",
#'                          assign_level = "l2",
#'                          edge_attribute = "duration")
#' # Print summary of the result
#' sprintf("The top level zone is split into %s zones at l2", length(unique(z$l2)))
#' 
#' # Calculate an adjacency matrix using travel duration
#' my_quickest_paths <- distances(graph = BristolBathGraph,
#'                                weights = edge_attr(BristolBathGraph,
#'                                                    "duration"))
#' # Randomly select two zones at `l2`, we wish to split them further
#' split_these_zones_further <- sample(unique(z$l2),
#'                                     size = 2,
#'                                     replace = FALSE)
#' # Run the algorithm again at one level down
#' z <- detect_communities(z = z,
#'                         g = BristolBathGraph,
#'                         at_level = "l2",
#'                         assign_level = "l3",
#'                         edge_attribute = "duration",
#'                         allow_exit_zone = TRUE,
#'                         m = my_quickest_paths,
#'                         within_zones = split_these_zones_further,
#'                         max_non_adjacent_path_length = 2)
#'                         
#' sprintf("L2 zone `%s` has been split into several smaller zone at the next level: %s", 
#'         split_these_zones_further[1],
#'         paste0(unique(subset(z, l2 == split_these_zones_further[1])$l3), collapse = ", "))
#' 
#' # The detect_communities() function can also be chained using %>% symbol, since 
#' # the first argument is always a `data.frame` object. Hierarchy of zones can be 
#' # easily created in this way.
#'
#' # If the following example, the l2 zones with 95 percentile travel time greater
#' # than 2 hours are split into smaller ones at l3.
#' z <- data.frame(name = igraph::vertex_attr(BristolBathGraph, "name"),
#'                 l1 = "SW England") %>%
#'   detect_communities(g = BristolBathGraph,
#'                      at_level = "l1",
#'                      assign_level = "l2",
#'                      edge_attribute = "duration") %>%
#'   detect_communities(g = BristolBathGraph,
#'                      at_level = "l2",
#'                      assign_level = "l3",
#'                      edge_attribute = "duration",
#'                      allow_exit_zone = TRUE,
#'                      m = my_quickest_paths,
#'                      within_zones = get_matrix_aggregate(
#'                          g = BristolBathGraph,
#'                          m = my_quickest_paths,
#'                          groups = z[["l2"]],
#'                          func = quantile,
#'                          probs = 0.95,
#'                          names = FALSE) %>% 
#'                        extract(.>(60*60*2)) %>%
#'                        names(),
#'                      max_non_adjacent_path_length = 2)
#' @references
#' \itemize{
#' \item Enclave and exclave \cr
#' \url{https://en.wikipedia.org/wiki/Enclave_and_exclave}
#' \item Louvain method for clustering \cr
#' \url{https://igraph.org/r/doc/cluster_louvain.html}
#' }
#' @export
detect_communities <- function(z,
                               g,
                               at_level,
                               assign_level,
                               edge_attribute,
                               allow_exit_zone = FALSE,
                               m = NULL,
                               within_zones = NULL,
                               max_non_adjacent_path_length = 2,
                               penalty = function(x) { base::log(scales::rescale(-x)+1)^0.1 }) {
  
  # If the vertex names are not present in the z data frame, create it
  if(!"name" %in% names(z)){
    base::stop("The data frame z must vertex name in the `name` column")
  } else {
    if (!base::identical(base::sort(z$name), 
                         base::sort(igraph::vertex_attr(g, "name")))) {
      base::stop("The name column in data frame z and vertex name of graph g must be the same")
    }
  }
  
  # Identify all zones at this level
  zones <- base::unique(z[[at_level]])
  
  # Process these zones if within_zones is non-NULL
  if(!base::is.null(within_zones)) {
    zones <- base::intersect(zones, within_zones)
  }
  
  if (allow_exit_zone) {
    # Calculate the length of all shortest paths
    nodes <- Matrix::Matrix(igraph::distances(g), sparse=TRUE)
    
    # Path longer than max_non_adjacent_path_length will be set to zero
    nodes[nodes > max_non_adjacent_path_length] <- 0
    
    # Convert dense matrix m into a sparse matrix
    m <- Matrix::Matrix(m, sparse=TRUE)
    
    # Remove matrix values of the longer paths
    m[nodes == 0] <- 0
  }
  
  # Create hash value for the assign_level
  z[[assign_level]] <- base::as.vector(base::sapply(base::as.character(z[[at_level]]), 
                                                    digest::digest))
  
  result <- base::do.call(rbind,
                base::lapply(zones, function(zone){
                  
      # Identify all the vertexes within this zone
      v <- z[z[[at_level]]==zone,]$name
      
      if (allow_exit_zone) {
        # Subgraph is calculated from adjacency matrix...
        # so that the best route is taken into account
        sg <- igraph::graph_from_adjacency_matrix(m[v,v],
                                                  mode = "undirected",
                                                  weighted = edge_attribute,
                                                  diag = FALSE)
      } else {
        # Compute a subgraph using all member vertexes within this zone
        sg <- igraph::induced_subgraph(g, v)
      }
            
      # Run the cluster algorithm
      communities <- igraph::cluster_louvain(graph = sg, 
                                             weights = penalty(igraph::edge_attr(sg, edge_attribute)))
      
      # Compute new hash values
      membership <- base::sapply(X = communities$membership, 
                                 FUN = function(x){ digest::digest(c(zone, x)) })
      
      # Assign the new hash values to the assign_level
      result <- base::with(z, {
        id <- base::match(v, z$name)
        z[id, assign_level] <- membership
        return(z[id,])
      })
      
      return(result)
  }))
  
  # Result may contain a subset of vertexes if within_zones is set to non-NULL
  # Need to combine with the remaining data
  result <- rbind(z[match(setdiff(z[["name"]], result[["name"]]), z[["name"]]),],
        result)
  
  # The result must be sorted before returning
  # The vertex name column should be same as the on in graph object
  return(result[match(result[["name"]], igraph::vertex_attr(g, "name")),])
}