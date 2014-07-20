# This test case can be used to validate cachematrix.R
#
# It uses the testthat library:
#   install.packages("testthat")
#   library(testthat)
#   source("cachematrix.R")
#   test_file("test-cachematrix.R")
#
test_that("cachematrix inverts a given matrix", {
  
  matA <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  matAI <- matrix(c(-2, 1, 1.5, -0.5), nrow = 2, ncol = 2)
  
  matB <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)
  matBI <- matrix(c(-2, 1.5, 1, -0.5), nrow = 2, ncol = 2)
  
  cache = makeCacheMatrix(matA)
  
  expect_that(cache$get(), equals(matA))
  expect_that(cache$getInverse(), equals(NULL))
  expect_that(cacheSolve(cache), equals(matAI))
  expect_that(cache$getInverse(), equals(matAI))
  
  cache$set(matB)
  expect_that(cache$get(), equals(matB))
  expect_that(cache$getInverse(), equals(NULL))
  expect_that(cacheSolve(cache), equals(matBI))
  expect_that(cache$getInverse(), equals(matBI))
})