library(GeosptNet)

test_that("Simple vertex aggregation", {
  # Arrange
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups = c(rep("A",200), rep("B",45)),
    func = sum)
  
  # Assert  
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A", "B"))
  expect_equal(object = as.numeric(result),
               expected = c(1203691, 170716))
})


test_that("Vertex aggregation, with function outputing named vector", {
  # Arrange
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups = c(rep("A",200), rep("B",45)),
    func = quantile)
  
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
                            3397.5, 
                            6281.0,
                            8578.0,
                            15411.0))
  expect_equal(object = as.vector(result[,2]),
               expected = c(0.0,
                            0.0, 
                            1303,
                            7248,
                            12172))
})


test_that("Vertex aggregation, with function outputing unnamed vector", {
  # Arrange
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    names = FALSE)
  
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
                            3397.5, 
                            6281.0,
                            8578.0,
                            15411.0))
  expect_equal(object = as.vector(result[,2]),
               expected = c(0.0,
                            0.0, 
                            1303,
                            7248,
                            12172))
})


test_that("Vertex aggregation, with function outputing unnamed vector of length 1", {
  # Arrange
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    probs = 0.95,
    names = FALSE)
  
  # Assert
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A", "B"))
  expect_equal(object = as.numeric(result),
               expected = c(12386.15, 10318.60))
})


test_that("Vertex aggregation, with function outputing named vector of length 1", {
  # Arrange
  
  # Action
  result <- get_vertex_attr_aggregate(
    g = BristolBathGraph,
    attr = "population",
    groups = c(rep("A",200), rep("B",45)),
    func = quantile,
    probs = 0.95,
    names = TRUE)
  
  # Assert
  expect_vector(result)
  expect_length(object = result, n = 2L)
  expect_named(object = result,
               expected = c("A.95%", "B.95%"))
  expect_equal(object = as.numeric(result),
               expected = c(12386.15, 10318.60))
})


test_that("Incorrect groups - NULL value", {
  # Arrange
  # Action
  # Assert
  expect_error(
    object = get_vertex_attr_aggregate(
      g = BristolBathGraph,
      attr = "population",
      groups = NULL,
      func = quantile,
      probs = 0.95,
      names = TRUE),
    regexp = "Length of attributes and length of groups are not identical")
})

test_that("Incorrect groups input - empty vector", {
  # Arrange
  # Action
  # Assert
  expect_error(
    object = get_vertex_attr_aggregate(
      g = BristolBathGraph,
      attr = "population",
      groups = c(),
      func = quantile,
      probs = 0.95,
      names = TRUE),
    regexp = "Length of attributes and length of groups are not identical")
})


test_that("Incorrect groups input - empty vector", {
  # Arrange
  # Action
  # Assert
  expect_error(
    object = get_vertex_attr_aggregate(
      g = BristolBathGraph,
      attr = "non_existent_attribute",
      groups =  c(rep("A",200), rep("B",45)),
      func = sum),
    regexp = "Attribute does not exist in this graph")
})


test_that("Custom function does not take any input", {
  # Arrange
  myfunc <- function(){ return(12345) }
  
  # Action
  # Assert
  expect_error(
    object = get_vertex_attr_aggregate(
      g = BristolBathGraph,
      attr = "population",
      groups =  c(rep("A",200), rep("B",45)),
      func = myfunc),
    regexp = NULL)
})

test_that("Custom function does not return any output", {
  # Arrange
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
