library(GeosptNet)

test_that("Simple matrix aggregation", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  result <- get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths,
    groups = c(rep("A",200), rep("B",45)),
    func = max
  )
  
  # Assert  
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A", "B"))
  expect_equal(object = as.numeric(result),
               expected = c(11032, 10337))
})


test_that("Incorrect graph size", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  # Assert  
  expect_error(get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths[1:10,1:5],
    groups = c(rep("A",200), rep("B",45)),
    func = max
  ),regexp = "Matrix m needs identical number of rows and columns")
  
})


test_that("Simple matrix aggregation, with function outputing named vector", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  result <- get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths,
    groups = c(rep("A",200), rep("B",45)),
    func = quantile
  )
  
  # Assert
  expect_true(object = is.matrix(result))
  expect_equal(object = dim(result),
               expected = c(5,2))
  expect_equal(object = colnames(result),
               expected = c("A", "B"))
  expect_equal(object = rownames(result),
               expected = c("0%", "25%", "50%", "75%", "100%"))
  expect_equal(object = as.vector(result[,1]),
               expected = c(0.0,
                            2244, 
                            3659,
                            5193,
                            11032))
  expect_equal(object = as.vector(result[,2]),
               expected = c(0,
                            2260, 
                            4119,
                            5944,
                            10337))
})



test_that("Simple matrix aggregation, with function outputing unnamed vector", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  result <- get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths,
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    names=FALSE
  )
  
  # Assert
  expect_true(object = is.matrix(result))
  expect_equal(object = dim(result),
               expected = c(5,2))
  expect_equal(object = colnames(result),
               expected = c("A", "B"))
  expect_equal(object = rownames(result),
               expected = NULL)
  expect_equal(object = as.vector(result[,1]),
               expected = c(0.0,
                            2244, 
                            3659,
                            5193,
                            11032))
  expect_equal(object = as.vector(result[,2]),
               expected = c(0,
                            2260, 
                            4119,
                            5944,
                            10337))
})


test_that("Simple matrix aggregation, with function outputing unnamed vector of length 1", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  result <- get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths,
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    probs = 0.95,
    names = FALSE
  )
  
  # Assert
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A", "B"))
  expect_equal(object = as.numeric(result),
               expected = c(7387.1, 7803.6))
})


test_that("Simple matrix aggregation, with function outputing named vector of length 1", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  result <- get_matrix_aggregate(
    g = BristolBathGraph,
    m = my_quickest_paths,
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    probs = 0.95,
    names = TRUE
  )
  
  # Assert
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A.95%", "B.95%"))
  expect_equal(object = as.numeric(result),
               expected = c(7387.1, 7803.6))
})


test_that("Incorrect groups - NULL value", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  # Assert
  expect_error(
    object = get_matrix_aggregate(
      g = BristolBathGraph,
      m = my_quickest_paths,
      groups = NULL,
      func = max),
    regexp = "Length of attributes and number of vertices of graph are not identical")
})

test_that("Incorrect groups - empty vector", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  
  # Action
  # Assert
  expect_error(
    object = get_matrix_aggregate(
      g = BristolBathGraph,
      m = my_quickest_paths,
      groups = c(),
      func = max),
    regexp = "Length of attributes and number of vertices of graph are not identical")
})


test_that("Ccustom function does not take any input", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  myfunc <- function(){ return(12345) }
  
  # Action
  # Assert
  expect_error(
    object = get_matrix_aggregate(
      g = BristolBathGraph,
      m = my_quickest_paths,
      groups = c(rep("A",200), rep("B",45)),
      func = myfunc),
    regexp = NULL)
})

test_that("Custom function does not return any output", {
  # Arrange
  library(igraph)
  my_quickest_paths <- distances(graph = BristolBathGraph,
                                 weights = edge_attr(BristolBathGraph,
                                                     "duration"))
  myfunc <- function(x){ return() }
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups =  c(rep("A",200), rep("B",45)),
    func = myfunc)
  
  # Assert
  expect_length(object = result, 
                n = 2)
  expect_true(object = is.list(result))
  expect_named(object = result,
               expected = c("A", "B"))
  expect_null(object = result[[1]])
  expect_null(object = result[[2]])
})
