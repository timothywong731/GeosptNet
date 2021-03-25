library(GeosptNet)
library(igraph)

test_that("Merge communities", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))

  # Action
  z <-  merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 20,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 0,
    cost_upper_threshold = 4,
    parent_level = "l1",
    penalty = function(x){x},
    verbose = FALSE)
  
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                size = 8L)
  expect_vector(object = z$l3,
                size = 8L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 1L)
  expect_vector(object = unique(subset(z,l1==1)$l2),
                ptype = character(),
                size = 1L)
})


test_that("Incorrect name column", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(NAME = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "The data frame z must vertex name in the `name` column")
})


test_that("Name column not identical", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = c("I","J","K","L","M","N","O","P"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("Name column not equal length", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name")[1:6],
                  l1 = c(1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("within_parents is non-NULL", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  z <-  merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 20,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 0,
    cost_upper_threshold = 4,
    within_parents = 1,
    parent_level = "l1",
    penalty = function(x){x},
    verbose = FALSE)
  
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                size = 8L)
  expect_vector(object = z$l3,
                size = 8L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 1L)
  expect_vector(object = unique(subset(z,l1==1)$l2),
                ptype = character(),
                size = 1L)
})


test_that("within_parents not exist in the data frame", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  z <-  merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 20,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 0,
    cost_upper_threshold = 4,
    within_parents = 2,
    parent_level = "l1",
    penalty = function(x){x},
    verbose = FALSE)
  
  
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                size = 8L)
  expect_vector(object = z$l3,
                size = 8L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 2L)
  expect_vector(object = unique(subset(z,l3==2)$l2),
                ptype = character(),
                size = 1L)
  expect_false(unique(subset(z,l3==2)$l2) == unique(subset(z,l3==3)$l2))
})


test_that("Wrong matrix size", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))[1:3,]
  
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "Matrix m needs identical number of rows and columns")
})


test_that("at_level equal to name", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "name",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("assign_level equal to name", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "name",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("Incorrect assign_level", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l3",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "assign_level, at_level and parent_level must be different")
})


test_that("Incorrect vertex_attribute", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "non_existent_attribute",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "vertex_attribute does not exist in graph")
})


test_that("Incorrect edge_attribute", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "non_existent_attribute",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){x},
      verbose = FALSE),
    regexp = "edge_attribute does not exist in graph")
})


test_that("Custom vertex_aggregate_func does not take any input", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(){x},
      verbose = FALSE),
    regexp = "unused argument")
})


test_that("Custom vertex_aggregate_func does not return any output", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = function(x){return()},
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "'list' object cannot be coerced to type 'double'")
})


test_that("vertex_aggregate_lower_threshold / vertex_aggregate_upper_threshold out of range", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = function(x){return()},
      vertex_aggregate_lower_threshold = 100,
      vertex_aggregate_upper_threshold = 0,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "vertex_aggregate_lower_threshold must be smaller than vertex_aggregate_upper_threshold")
})


test_that("vertex_aggregate_lower_threshold / vertex_aggregate_upper_threshold are NULL", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = function(x){return()},
      vertex_aggregate_lower_threshold = NULL,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "argument is of length zero")
})


test_that("Custom cost_aggregate_func does not take any input", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = function(){return(1)},
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "unused argument")
})


test_that("Custom cost_aggregate_func does not return any output", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = function(x){return()},
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "'list' object cannot be coerced to type 'double'")
})


test_that("cost_lower_threshold / cost_upper_threshold out of range", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 10,
      cost_upper_threshold = 0,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "cost_lower_threshold must be smaller than cost_upper_threshold")
})


test_that("cost_lower_threshold / cost_upper_threshold are NULL", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = function(x){return()},
      cost_lower_threshold = NULL,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "argument is of length zero")
})


test_that("parent_level does not exist", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,1,2,2,2,2,2,2,2,2,2,1,2,2,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  l3 = c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,4,5,5,5,5,4,4,4,6,6,4,4,7,6,7,7,6,6,6,6,6,4,4,4,8,8,8,9,9,9,9,1,3,10,10,10,10,10,10,10,4,4,9,4,4,11,11,9,5,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,12,12,12,12,12,12,12,12,14,14,14,14,14,14,14,14,15,14,14,15,15,15,15,15,13,13,13,13,13,13,13,13,13,13,13,3,12,3,3,3,3,3,3,3,12,3,3,3,3,3,14,14,14,16,3,14,16,16,16,12,12,12,15,15,15,12,15,9,15,9,11,11,9,9,9,11,11,11,11,11,11,11,11,11,11,11,9,11,9,3,14,16,12,16,16,16,16,16,16,9,9,4,12,1,2,8,6,2,3,5,4,5,5,4,7,6,6,4,7,6,4,4,8,3,3,10,4,11,11,5,11,11,12,13,12,14,12,12,12,15,13,14,14,14,14,14,16,15,13,9,3,3,16,15,15,9,11,16,16),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = BristolBathGraph,
      m = my_quickest_paths,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "population",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 100000,
      edge_attribute = "duration",
      cost_aggregate_func = quantile,
      cost_lower_threshold = 60*60*0.1,
      cost_upper_threshold = Inf,
      parent_level = "non_existent_level",
      cost_aggregate_args = list(probs=0.95,
                                 names=FALSE),
      verbose = FALSE),
    regexp = "parent_level does not exist in data frame z")
})


test_that("assign_level, at_level and parent_level all identical", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l3",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l3",
      penalty = function(x){return(x)},
      verbose = FALSE),
    regexp = "assign_level, at_level and parent_level must be different")
})


test_that("Custom penalty does not take any input", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_error(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(){return(1)},
      verbose = FALSE),
    regexp = "unused argument")
})


test_that("Custom penalty does not return any output", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  z <- merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 20,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 0,
    cost_upper_threshold = 4,
    parent_level = "l1",
    penalty = function(x){return()},
    verbose = FALSE)
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                size = 8L)
  expect_vector(object = z$l3,
                size = 8L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 1L)
  expect_vector(object = unique(subset(z,l1==1)$l2),
                ptype = character(),
                size = 1L)
})


test_that("Additional argument for vertex_aggregate_args", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,NA,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  z <- merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 20,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 0,
    cost_upper_threshold = 4,
    parent_level = "l1",
    vertex_aggregate_args = list(na.rm=TRUE),
    penalty = function(x){return(x)},
    verbose = FALSE)
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                size = 8L)
  expect_vector(object = z$l3,
                size = 8L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 1L)
  expect_vector(object = unique(subset(z,l1==1)$l2),
                ptype = character(),
                size = 1L)
})


test_that("Set verbose=TRUE", {
  # Arrange
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,NA,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"duration"))
  
  # Action
  # Assert
  expect_output(
    object = merge_communities(
      z = z,
      g = g,
      m = m,
      at_level = "l3",
      assign_level = "l2",
      vertex_attribute = "vertex_value",
      vertex_aggregate_func = sum,
      vertex_aggregate_lower_threshold = 0,
      vertex_aggregate_upper_threshold = 20,
      edge_attribute = "edge_value",
      cost_aggregate_func = max,
      cost_lower_threshold = 0,
      cost_upper_threshold = 4,
      parent_level = "l1",
      penalty = function(x){return(x)},
      verbose = TRUE),
    regexp = ".\nTotal zones merged: 1")
})


test_that("Merging islands", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F, I)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                             1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3,1)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3,4),
                  stringsAsFactors = FALSE)
  
  m <- distances(graph = g, weights = edge_attr(g,"edge_value"))
  
  # Action
  z <- merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 100,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 2,
    cost_upper_threshold = Inf,
    parent_level = "l1",
    vertex_aggregate_args = list(na.rm = TRUE),
    penalty = function(x){return(x)},
    verbose = FALSE)
  
  # Assert
  expect_vector(
    object = unique(z$l2),
    ptype = character(),
    size = 2L)
})


test_that("Checking cost threshold limits", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F, I)
  edge_attr(g, "edge_value") <- c(1,1,1,50,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3,1)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1,1),
                  l3 = c(2,2,2,2,3,3,3,3,4),
                  stringsAsFactors = FALSE)
  
  m <- distances(graph = g, weights = edge_attr(g,"edge_value"))
  
  # Action
  z <- merge_communities(
    z = z,
    g = g,
    m = m,
    at_level = "l3",
    assign_level = "l2",
    vertex_attribute = "vertex_value",
    vertex_aggregate_func = sum,
    vertex_aggregate_lower_threshold = 0,
    vertex_aggregate_upper_threshold = 100,
    edge_attribute = "edge_value",
    cost_aggregate_func = max,
    cost_lower_threshold = 100,
    cost_upper_threshold = 200,
    parent_level = "l1",
    vertex_aggregate_args = list(na.rm = TRUE),
    penalty = function(x){return(x)},
    verbose = FALSE)
  
  # Assert
  expect_vector(
    object = unique(z$l2),
    ptype = character(),
    size = 3L)
  expect_vector(
    object = unique(subset(z, l3==2)$l2),
    ptype = character(),
    size = 1L)
  expect_vector(
    object = unique(subset(z, l3==3)$l2),
    ptype = character(),
    size = 1L)
  expect_vector(
    object = unique(subset(z, l3==4)$l2),
    ptype = character(),
    size = 1L)
})
