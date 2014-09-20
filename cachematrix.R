## A simple protocol that allows the memoization 
## of the calculation of the inverse of a matrix

## Example usage:
##
## > D <- 10000
## > foo <- matrix( sample(0:1, D*D, replace=T), D, D)
## > cache_matrix <- makeCacheMatrix(foo)
## > system.time(cacheSolve(cache_matrix))
##    user  system elapsed 
##   1.900   0.008   1.907  
## > system.time(cacheSolve(cache_matrix))
##    user  system elapsed 
##   0.000   0.001   0.000 

## Creates a closure around a matrix that allows the memoization of its inverse

makeCacheMatrix <- function(mtx = matrix()) {
  memoized_inverse <- NULL
  
  set <- function(y) {
    mtx <<- y
    memoized_inverse <<- NULL
  }
  
  get <- function() { mtx }
  
  setInverse <- function(mean) { memoized_inverse <<- mean }
  
  getInverse <- function() { memoized_inverse }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## expects a makeCacheMatrix, uses it memoized inverse if available else caches
## and returns its inverese.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(is.null(inverse)) {
    inverse <- solve(x$get())
    x$setInverse(inverse)
  }
  inverse
}
