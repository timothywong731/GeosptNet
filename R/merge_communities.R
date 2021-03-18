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
  within_level,
  within_zones = NULL,
  penalty = function(x){ base::log(scales::rescale(-x)+1)^0.1 },
  cost_aggregate_args = list(),
  vertex_aggregate_args = list()
){
  
  # Create hash value for the assign_level
  z[[assign_level]] <- base::as.vector(base::sapply(base::as.character(z[[at_level]]), 
                                                    digest::digest))
  
  n2 <- -1

  while (TRUE) {
    
    reference <- base::data.frame(
      z,
      hash = base::sapply(base::seq_along(z[[1]]), 
                          function(i){ digest::digest(c(z[i,][[within_level]],
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
      zonal_aggregate <- zonal_aggregate[base::names(zonal_aggregate) %in% base::unique(reference[reference[[within_level]] %in% within_zones,]$hash)]
    }
    
    zone_hashes <- base::sort(zonal_aggregate[
      zonal_aggregate > vertex_aggregate_lower_threshold &
        zonal_aggregate < vertex_aggregate_upper_threshold])
    
    for (zone_hash in base::names(zone_hashes)) {
      # Identify the zone name and parent zone name
      zone <- base::unique(reference[reference$hash == zone_hash, ][[assign_level]])
      parent_zone <- base::unique(z[z[[assign_level]] == zone, ][[within_level]])
      
      # Identify all vertex names of this zone
      zone_vertexes <- z[z[[assign_level]] == zone, ]$name
      
      # Identify all vertexes in parent zone
      parent_vertexes <- z[z[[within_level]] == parent_zone,]$name
      
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
        proposed_membership <- base:::factor(
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
        break
      } else{
        # Move to next zone if cost is above cost_threshold
        # Do not merge
        next
      }
    }
  }
  return(z)
}