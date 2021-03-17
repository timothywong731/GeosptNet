#' @export
detect_communities <- function(z,
                               g,
                               m,
                               by_level,
                               assign_level,
                               edge_attribute,
                               within_zones = NULL,
                               allow_exit_zone = TRUE,
                               max_non_adjacent_path_size = 2,
                               penalty = function(x){ log(scales::rescale(-x)+1)^0.1 }){

    
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
  zones <- base::unique(z[[by_level]])
  
  # Process these zones if within_zones is non-NULL
  if(!is.null(within_zones)) {
    zones <- intersect(zones, within_zones)
  }
  
  
  
  # Create hash value for the assign_level
  z[[assign_level]] <- base::as.vector(base::sapply(base::as.character(z[[by_level]]), 
                                                    digest::digest))
  do.call(rbind,
    lapply(zones, function(zone){
      # Identify all the vertexes within this zone
      v <- z[z[[by_level]]==zone,]$name
      
      if (allow_exit_zone) {
        
        # Calculate the length of all shortest paths
        nodes <- Matrix::Matrix(igraph::distances(g), sparse=TRUE)
        
        # Path longer than max_non_adjacent_path_size will be set to zero
        nodes[nodes > max_non_adjacent_path_size] <- 0
        
        m <- Matrix::Matrix(m, sparse=TRUE)
        m[nodes == 0] <- 0
        
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
      result <- with(z, {
        id <- base::match(v, z$name)
        z[id, assign_level] <- membership
        return(z[id,])
      })
      
      return(result)
  }))
}