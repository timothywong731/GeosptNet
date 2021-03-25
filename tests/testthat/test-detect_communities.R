library(GeosptNet)
library(igraph)

test_that("Communities detection", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  # Action
  z <-  detect_communities(z = z,
                           g = g,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "edge_value",
                           penalty = function(x){scales::rescale(-x)},)
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 8L)
  expect_length(object = unique(z$l2),
                n = 2L)
  expect_identical(
    object = z$name,
    expected = vertex_attr(g, "name"))
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
                  stringsAsFactors = FALSE)
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = g,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "edge_value",
    penalty = function(x){scales::rescale(-x)}),
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
                  stringsAsFactors = FALSE)
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = g,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "duration",
    penalty = function(x){scales::rescale(-x)}),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("Data frame not equal length", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name")[1:6],
                  l1 = c(1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = BristolBathGraph,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "duration",
    penalty = function(x){scales::rescale(-x)}),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("within_zones is non-NULL", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  
  # Action
  z <-  detect_communities(z = z,
                           g = g,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "edge_value",
                           within_zones = 1,
                           penalty = function(x){scales::rescale(-x)})
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 8L)
  expect_length(object = unique(z$l2),
                n = 2L)
  expect_identical(
    object = z$name,
    expected = vertex_attr(g, "name"))
})


test_that("within_zones not exist in the data frame", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  
  # Action
  z <-  detect_communities(z = z,
                           g = g,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "edge_value",
                           within_zones = 999,
                           penalty = function(x){scales::rescale(-x)})
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 8L)
  expect_length(object = unique(z$l2),
                n = 1L)
  expect_identical(
    object = z$name,
    expected = vertex_attr(g, "name"))
})


test_that("allow_exit_zone is TRUE", {
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
    weights = edge_attr(g,"edge_value"))
  
  
  # Action
  z <-  detect_communities(z = z,
                           g = g,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "edge_value",
                           allow_exit_zone = TRUE,
                           m = m,
                           penalty = function(x){scales::rescale(-x)},
                           max_non_adjacent_path_length = 2)
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 8L)
  expect_vector(object = z$l1,
                size = 8L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 8L)
  expect_length(object = unique(z$l2),
                n = 2L)
  expect_identical(
    object = z$name,
    expected = vertex_attr(g, "name"))
})


test_that("wrong matrix size", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))[1:5,1:3]
  
  
  # Action
  # Assert
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "edge_value",
                                allow_exit_zone = TRUE,
                                m = m,
                                penalty = function(x){scales::rescale(-x)},
                                max_non_adjacent_path_length = 2),
    regexp = "index larger than maximal 15")
})


test_that("max_non_adjacent_path_length less than 1", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))
  
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "edge_value",
                                allow_exit_zone = TRUE,
                                m = m,
                                penalty = function(x){scales::rescale(-x)},
                                max_non_adjacent_path_length = 0),
    regexp = "max_non_adjacent_path_length must be greater then or equal to 1")
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
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l1",
                                edge_attribute = "edge_value",
                                allow_exit_zone = TRUE,
                                penalty = function(x){scales::rescale(-x)},
                                m = m),
    regexp = "assign_level and at_level cannot be identical")
})


test_that("assign_level equal to 'name'", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "name",
                                edge_attribute = "edge_value",
                                allow_exit_zone = TRUE,
                                penalty = function(x){scales::rescale(-x)},
                                m = m),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("at_level equal to 'name'", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "name",
                                assign_level = "l2",
                                edge_attribute = "edge_value",
                                allow_exit_zone = TRUE,
                                penalty = function(x){scales::rescale(-x)},
                                m = m),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("edge_attribute not exist", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "non_existent_attribute",
                                penalty = function(x){scales::rescale(-x)}),
    regexp = "edge_attribute does not exist in graph")
})


test_that("custom penalty function does not take any input", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "edge_value",
                                penalty = function() { return(1:5) }),
    regexp = "unused argument")
})


test_that("custom penalty function does not return any output", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  
  # Action
  z <- detect_communities(z = z,
                          g = g,
                          at_level = "l1",
                          assign_level = "l2",
                          edge_attribute = "edge_value",
                          penalty = function(x) { return() })
  
  # Assert
  expect_vector(
    object = unique(z$l2),
    ptype = character(),
    size = 2L)
})


test_that("custom penalty function returns short output", {
  # Arrange
  g <- graph_from_literal(A--B, B--C, C--D, C--A, D--A, D--B,
                          A--E,
                          E--F, F--G, G--H, G--E, H--E, H--F)
  edge_attr(g, "edge_value") <- c(1,1,1,1,1,1,
                                  1,1,1,1,1,1,1)
  vertex_attr(g,"vertex_value") <- c(2,2,2,2,3,3,3,3)
  
  z <- data.frame(name = vertex_attr(g,"name"),
                  l1 = c(1,1,1,1,1,1,1,1),
                  stringsAsFactors = FALSE)
  m <- distances(
    graph = g,
    weights = edge_attr(g,"edge_value"))
  
  # Action
  # Assert
  expect_error(
    object = detect_communities(z = z,
                                g = g,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "edge_value",
                                penalty = function(x) { return(1:5) }),
    regexp = "multi-level community detection: weight vector too short, Invalid value")
})
