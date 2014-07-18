# -------------------------------------------------------------------
# rprog-005
# Programming Assignment 2
#   https://class.coursera.org/rprog-005/human_grading/view/courses/972576/assessments/3
# -------------------------------------------------------------------

#' Creates a special "matrix" object that can cache its inverse
#' 
makeCacheMatrix <- function(x = matrix()) {
  matI <- NULL
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    matI <<- NULL
  }
  
  # get the value of the inverse
  getInverse <- function() matI
  
  # set the value of the inverse
  setInverse <- function(inverse) {
    matI <<- inverse
  }
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


#' Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then the cachesolve should retrieve the inverse from the cache.
#' 
#' @return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  mat <- x$getInverse()
  
  if (!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  mat <- solve(x$get(), ...)
  x$setInverse(mat)
  mat
}