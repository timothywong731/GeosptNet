#' @title Merge Communities in a Graph
#' @description Merge smaller zones to create larger ones.
#' @param z A `data frame` object containing character column `name` which contains all vertex names in graph `g`, and at least one named column of characters indicating zones to be merged.
#' @param g An `igraph` object
#' @param m An adjacency matrix of numeric values. The number of column and rows must be identical. All rows and columns must be named. Ideally this is a `N*N` shortest path distance matrix.
#' @param at_level Character value indicating the level to run merge algorithm at. The level must exist as a column of character vector in the data frame `z`.
#' @param assign_level Character value indicating the level to assign the merge algorithm output. 
#' @param vertex_attribute Character value which indicates the vertex attribute to be used for merge algorithm.
#' @param vertex_aggregate_func Function to aggregate vertex attributes. Optional `...` parameters may be passed onto this function in `vertex_aggregate_args`.
#' @param vertex_aggregate_lower_threshold Numeric value indicating the lower threshold of the vertex aggregate. Zones with aggregate value greater than this value will attempt to merge with an adjacent zone.
#' @param vertex_aggregate_upper_threshold Numeric value indicating the upper threshold of the vertex aggregate. Zones with aggregate value smaller than this value will attempt to merge with an adjacent zone.
#' @param edge_attribute Character value which indicates the edge attribute to be used for merge algorithm.
#' @param cost_aggregate_func Function to aggregate edge attributes. Optional `...` parameters may be passed onto this function in `cost_aggregate_args`.
#' @param cost_lower_threshold Numeric value indicating the lower threshold of the cost aggregate. Zones with aggregate value greater than this value will attempt to merge with an adjacent zone.
#' @param cost_upper_threshold Numeric value indicating the upper threshold of the cost aggregate. Zones with aggregate value smaller than this value will attempt to merge with an adjacent zone.
#' @param parent_level Character value indicating the parent level to stay within. Zones defined at `at_level` will attempt to merge with an adjacent zone only if the target adjacent zone shares the same parent zone at `parent_level`.
#' @param within_zones Optional parameter. Vector of characters indicating names of zones to detect community within. Default value is `NULL` which will process all zones at the level specified in `at_level`.
#' @param penalty A function which takes one numeric vector as input. Implements appropriate transformation and penalty. The output is used to calculate graph modularity in the algorithm. Please implement your own penalty function depending on what `edge_attribute` is being used.
#' @param vertex_aggregate_args A named list of additional parameters to be passed onto `vertex_aggregate_func`.
#' @param cost_aggregate_args A named list of additional parameters to be passed onto `cost_aggregate_func`.
#' @param verbose Logical value indicating whether to print out progress and summary on the console. Default to `TRUE`.
#' @return A `data frame` object
#' @author Timothy Wong, \email{timothy.wong@@hotmail.co.uk}
#' @examples
#' library(igraph)
#' library(dplyr)
#' library(magrittr)
#' # Calculate adjacency matrix
#' my_quickest_paths <- distances(graph = BristolBathGraph,
#'                                weights = edge_attr(BristolBathGraph,
#'                                                    "duration"))
#'
#' # This will split the top level zone `l1` into smaller zones at `l2`
#' # The `l2` zones with 95 percentile travel time greater than 0.5 hour 
#' # are split into smaller ones which will become `l4`.
#' #
#' # The `merge_communities()` function in this example will merge small zones
#' # at `l4` back into larger ones at `l3`. It will attempt to merge `l4` zones 
#' # with total population between 0-75000. Those zones will attempt to merge
#' #  with the most optimally-connected adjacent zone, as indicated by the
#' #  modularity function. They will only merge if the adjacency cost matrix
#' # (95 percentile travel duration) is above 2 hours and below +Inf. 
#' z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
#'                 l1 = "SW England",
#'                 stringsAsFactors = FALSE) %>%
#'   detect_communities(g = BristolBathGraph,
#'                      at_level = "l1",
#'                      assign_level = "l2",
#'                      edge_attribute = "duration") %>%
#'   detect_communities(g = BristolBathGraph,
#'                      at_level = "l2",
#'                      assign_level = "l4",
#'                      edge_attribute = "duration",
#'                      allow_exit_zone = TRUE,
#'                      m = my_quickest_paths,
#'                      within_zones = get_matrix_aggregate(
#'                        g = BristolBathGraph,
#'                        m = my_quickest_paths,
#'                        groups = .[["l2"]],
#'                        func = quantile,
#'                        probs = 0.95,
#'                        names = FALSE) %>% 
#'                        extract(.>(60*60*0.5)) %>%
#'                        names(),
#'                      max_non_adjacent_path_length = 2) %>%
#'   merge_communities(g = BristolBathGraph,
#'                     m = my_quickest_paths,
#'                     at_level = "l4",
#'                     assign_level = "l3",
#'                     vertex_attribute = "population",
#'                     vertex_aggregate_func = sum,
#'                     vertex_aggregate_lower_threshold = 0,
#'                     vertex_aggregate_upper_threshold = 75000,
#'                     edge_attribute = "duration",
#'                     cost_aggregate_func = quantile,
#'                     cost_lower_threshold = (60*60*2),
#'                     cost_upper_threshold = Inf,
#'                     parent_level = "l2",
#'                     cost_aggregate_args = list(probs = 0.95, 
#'                                                names = FALSE))
#' @references
#' \itemize{
#' \item Modularity \cr
#' \url{https://igraph.org/r/doc/modularity.igraph.html}
#' }
#' @export
merge_communities <- function(
  z,
  g,
  m,
  at_level,
  assign_level,
  vertex_attribute,
  vertex_aggregate_func,
  vertex_aggregate_lower_threshold,
  vertex_aggregate_upper_threshold,
  edge_attribute,
  cost_aggregate_func,
  cost_lower_threshold,
  cost_upper_threshold,
  parent_level,
  within_zones = NULL,
  penalty = function(x){ base::log(scales::rescale(-x)+1)^0.1 },
  vertex_aggregate_args = list(),
  cost_aggregate_args = list(),
  verbose = TRUE
){
  
  # Create hash value for the assign_level
  z[[assign_level]] <- base::as.vector(base::sapply(base::as.character(z[[at_level]]), 
                                                    digest::digest))
  
  counter <- 0L
  
  n2 <- -1

  while (TRUE) {
    
    reference <- base::data.frame(
      z,
      hash = base::sapply(base::seq_along(z[[1]]), 
                          function(i){ digest::digest(c(z[i,][[parent_level]],
                                                        z[i,][[assign_level]])) }))
    
    
    n <- base::length(base::unique(z[[assign_level]]))
    
    if (n2 == n) {
      # Merging has stopped
      break
    } else {
      n2 <- n
    }
    
    zonal_aggregate <- do.call(get_vertex_attr_aggregate,
                               c(list(g = g,
                                      attr = vertex_attribute,
                                      groups = reference$hash,
                                      func = vertex_aggregate_func),
                                 vertex_aggregate_args))
    
    if (!base::is.null(within_zones)) {
      # Merge only these zones
      zonal_aggregate <- zonal_aggregate[base::names(zonal_aggregate) %in% base::unique(reference[reference[[parent_level]] %in% within_zones,]$hash)]
    }
    
    zone_hashes <- base::sort(zonal_aggregate[
      zonal_aggregate > vertex_aggregate_lower_threshold &
        zonal_aggregate < vertex_aggregate_upper_threshold])
    
    for (zone_hash in base::names(zone_hashes)) {
      # Identify the zone name and parent zone name
      zone <- base::unique(reference[reference$hash == zone_hash, ][[assign_level]])
      parent_zone <- base::unique(z[z[[assign_level]] == zone, ][[parent_level]])
      
      # Identify all vertex names of this zone
      zone_vertexes <- z[z[[assign_level]] == zone, ]$name
      
      # Identify all vertexes in parent zone
      parent_vertexes <- z[z[[parent_level]] == parent_zone,]$name
      
      # Identify all adjacent vertexes of this zone
      adjacent_vertexes <- base::unique(
        base::as.vector(
          base::unlist(
            base::sapply(igraph::adjacent_vertices(graph = g,
                                             v = zone_vertexes), function(x){x$name}))))
      
      # Identify all adjacent zones except itself, for those which are within parent zone
      adjacent_zones <- base::setdiff(base::unique(z[base::match(base::intersect(parent_vertexes, adjacent_vertexes), z$name),][[assign_level]]), zone)
      
      if (base::length(adjacent_zones) < 1) {
        # There are no adjacent zones. This is probably an island.
        next
      }
      
      # Calculate modularity for each adjacent zone
      proposals <- base::lapply(adjacent_zones, function(adjacent_zone){
        # Create a vector for proposed membership
        proposed_membership <- base::factor(
          base::replace(x = z[[assign_level]],
                        list = z[[assign_level]] == adjacent_zone | z[[assign_level]] == zone,
                        values = zone))
        
        # Calculate modularity
        proposed_modularity <- igraph::modularity(
          x = g, 
          membership = proposed_membership, 
          weights = penalty(igraph::edge_attr(g, edge_attribute)))
        
        return(base::list(adjacent_zone = adjacent_zone, 
                          membership = proposed_membership, 
                          modularity = proposed_modularity))
      })
      
      # The proposal with highest modularity is the best option
      best_proposal_id <- base::which.max(base::sapply(proposals, function(x){x$modularity}))
      
      # Calculate costs for all zones
      costs <- do.call(get_matrix_aggregate,
                       c(list(g = g,
                              m = m,
                              groups = proposals[[best_proposal_id]]$membership,
                              func = cost_aggregate_func),
                         cost_aggregate_args))
      
      if (costs[names(costs) == zone] > cost_lower_threshold &
          costs[names(costs) == zone] < cost_upper_threshold) {
        # Merge zones if the cost is within the range between cost_lower_threshold and cost_upper_threshold
        z[[assign_level]] <- base::as.character(proposals[[best_proposal_id]]$membership)
        # Increment counter
        counter <- counter + 1
        if (verbose) { cat(".") }
        if (verbose && counter %% 50 == 0) { cat("\n") }
        
        break
      } else{
        # Move to next zone if cost is above cost_threshold
        # Do not merge
        next
      }
    }
  }
  if (verbose) { cat(sprintf("\n Total zones merged: %s\n", counter)) }
  return(z)
}