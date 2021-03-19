library(GeosptNet)
library(igraph)

test_that("Communities detection", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = "SW England",
                  stringsAsFactors = FALSE)
  
  # Action
  z <-  detect_communities(z = z,
                           g = BristolBathGraph,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "duration")
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l1,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 245L)
  expect_length(object = unique(z$l2),
                n = 10L)
})


test_that("Incorrect name column", {
  # Arrange
  z <- data.frame(NAME = vertex_attr(BristolBathGraph, "name"),
                  l1 = "SW England",
                  stringsAsFactors = FALSE)
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = BristolBathGraph,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "duration"),
    regexp = "The data frame z must vertex name in the `name` column")
})


test_that("Name column not identical", {
  # Arrange
  z <- data.frame(name = sample(LETTERS,size = 245L,replace = TRUE),
                  l1 = "SW England",
                  stringsAsFactors = FALSE)
  
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = BristolBathGraph,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "duration"),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("Name column not equal length", {
  # Arrange
  z <- data.frame(name = sample(LETTERS,size = 145L,replace = TRUE),
                  l1 = "SW England",
                  stringsAsFactors = FALSE)
  
  
  # Action
  # Assert
  expect_error(object = detect_communities(
    z = z,
    g = BristolBathGraph,
    at_level = "l1",
    assign_level = "l2",
    edge_attribute = "duration"),
    regexp = "The name column in data frame z and vertex name of graph g must be the same")
})


test_that("within_zones is non-NULL", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  z <-  detect_communities(z = z,
                           g = BristolBathGraph,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "duration",
                           within_zones = "A")
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l1,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 245L)
  expect_length(object = unique(z$l1),
                n = 2L)
  expect_length(object = length(unique(subset(z, l1=="B")$l2)),
                n = 1L)
  expect_gt(object = length(unique(subset(z, l1=="A")$l2)),
            expected = 1L)
})


test_that("within_zones not exist in the data frame", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  z <-  detect_communities(z = z,
                           g = BristolBathGraph,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "duration",
                           within_zones = "C")
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l1,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 245L)
  expect_length(object = unique(z$l1),
                n = 2L)
  expect_length(object = unique(z$l2),
                n = 2L)
  expect_length(object = length(unique(subset(z, l1=="B")$l2)),
                n = 1L)
  expect_length(object = length(unique(subset(z, l1=="A")$l2)),
                n = 1L)
})


test_that("allow_exit_zone is TRUE", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  z <-  detect_communities(z = z,
                           g = BristolBathGraph,
                           at_level = "l1",
                           assign_level = "l2",
                           edge_attribute = "duration",
                           allow_exit_zone = TRUE,
                           m = my_quickest_paths,
                           max_non_adjacent_path_length = 2)
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l1,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l2,
                ptype = character(),
                size = 245L)
  expect_length(object = unique(z$l1),
                n = 2L)
  expect_gt(object = length(unique(subset(z, l1=="B")$l2)),
            expected = 1L)
  expect_gt(object = length(unique(subset(z, l1=="A")$l2)),
            expected = 1L)
})


test_that("wrong matrix size", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  my_quickest_paths <- my_quickest_paths[1:5, 1:10]
  
  # Action
  # Assert
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "duration",
                                allow_exit_zone = TRUE,
                                m = my_quickest_paths,
                                max_non_adjacent_path_length = 2),
    regexp = "index larger than maximal 50")
})


test_that("max_non_adjacent_path_length less than 1", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "duration",
                                allow_exit_zone = TRUE,
                                m = my_quickest_paths,
                                max_non_adjacent_path_length = 0),
    regexp = "max_non_adjacent_path_length must be greater then or equal to 1")
})


test_that("Incorrect assign_level", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l1",
                                edge_attribute = "duration",
                                allow_exit_zone = TRUE,
                                m = my_quickest_paths),
    regexp = "assign_level and at_level cannot be identical")
})


test_that("assign_level equal to 'name'", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "name",
                                edge_attribute = "duration",
                                allow_exit_zone = TRUE,
                                m = my_quickest_paths),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("at_level equal to 'name'", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "name",
                                assign_level = "l2",
                                edge_attribute = "duration",
                                allow_exit_zone = TRUE,
                                m = my_quickest_paths),
    regexp = "assign_level and at_level cannot be 'name'")
})


test_that("edge_attribute not exist", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "non_existent_attribute"),
    regexp = "edge_attribute does not exist in graph")
})


test_that("custom penalty function does not take any input", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "duration",
                                penalty = function() { return(1:5) }),
    regexp = "unused argument")
})


test_that("custom penalty function does not return any output", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  result <- detect_communities(z = z,
                               g = BristolBathGraph,
                               at_level = "l1",
                               assign_level = "l2",
                               edge_attribute = "duration",
                               penalty = function(x) { return() })
  
  # Assert
  expect_length(
    object = unique(result$l2),
    n = 58)
})


test_that("custom penalty function returns short output", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(rep("A",200), rep("B",45)),
                  stringsAsFactors = FALSE)
  
  # Action
  # Assert
  expect_error(
    object = detect_communities(z = z,
                                g = BristolBathGraph,
                                at_level = "l1",
                                assign_level = "l2",
                                edge_attribute = "duration",
                                penalty = function(x) { return(runif(100)) }),
    regexp = "multi-level community detection: weight vector too short, Invalid value")
})
