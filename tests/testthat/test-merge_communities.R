library(GeosptNet)
library(igraph)

test_that("Communities detection", {
  # Arrange
  z <- data.frame(name = vertex_attr(BristolBathGraph, "name"),
                  l1 = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,1,2,2,2,2,2,2,2,2,2,1,2,2,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  l3 = c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,4,5,5,5,5,4,4,4,6,6,4,4,7,6,7,7,6,6,6,6,6,4,4,4,8,8,8,9,9,9,9,1,3,10,10,10,10,10,10,10,4,4,9,4,4,11,11,9,5,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,12,12,12,12,12,12,12,12,14,14,14,14,14,14,14,14,15,14,14,15,15,15,15,15,13,13,13,13,13,13,13,13,13,13,13,3,12,3,3,3,3,3,3,3,12,3,3,3,3,3,14,14,14,16,3,14,16,16,16,12,12,12,15,15,15,12,15,9,15,9,11,11,9,9,9,11,11,11,11,11,11,11,11,11,11,11,9,11,9,3,14,16,12,16,16,16,16,16,16,9,9,4,12,1,2,8,6,2,3,5,4,5,5,4,7,6,6,4,7,6,4,4,8,3,3,10,4,11,11,5,11,11,12,13,12,14,12,12,12,15,13,14,14,14,14,14,16,15,13,9,3,3,16,15,15,9,11,16,16),
                  stringsAsFactors = FALSE)
  
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))

  # Action
  z <-  merge_communities(z = z,
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
                          parent_level = "l1",
                          cost_aggregate_args = list(probs=0.95,
                                                     names=FALSE),
                          verbose = FALSE)
  
  
  # Assert
  expect_type(object = z,
              type = "list")
  expect_vector(object = z$name,
                ptype = character(),
                size = 245L)
  expect_vector(object = z$l1,
                size = 245L)
  expect_vector(object = z$l2,
                size = 245L)
  expect_vector(object = z$l3,
                size = 245L)
  expect_vector(object = unique(z$l2),
                ptype = character(),
                size = 9L)
  expect_identical(object = sum(table(z$l2,  z$l1)[,1]),
                   expected = 166L)
  expect_identical(object = sum(table(z$l2,  z$l1)[,2]),
                   expected = 79L)
  expect_identical(object = sum(table(z$l2,  z$l1)[,1] == 0),
                   expected = 3L)
  expect_identical(object = sum(table(z$l2,  z$l1)[,2] == 0),
                   expected = 6L)
  expect_identical(object = sum(table(z$l3,  z$l1)[,1]),
                   expected = 166L)
  expect_identical(object = sum(table(z$l3,  z$l1)[,2]),
                   expected = 79L)
  expect_identical(object = sum(table(z$l3,  z$l1)[,1] == 0),
                   expected = 8L)
  expect_identical(object = sum(table(z$l3,  z$l1)[,2] == 0),
                   expected = 8L)
  expect_vector(object = unique(subset(z,l1==1)$l2),
                ptype = character(),
                size = 6L)
  expect_vector(object = unique(subset(z,l1==2)$l2),
                ptype = character(),
                size = 3L)
})
