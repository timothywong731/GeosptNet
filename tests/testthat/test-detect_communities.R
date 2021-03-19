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
