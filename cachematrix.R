## This file contains two functions that allow storing the inverse of a matrix.
## Since inverting a matrix can be a costly operation, this allows performance
## gains when we need to use the inverse multiple times.

## To use this caching system, store a matrix m in a new object x by running
## x <- makeCacheMatrix(m). You can access that matrix calling x$get(). Then,
## to get the inverse, run cacheSolve(x). Now every time you run cacheSolve(x)
## or x$getInverse() you get a cached inverse matrix.


## This function receives a matrix x and returns a new object (a list) to be
## used by function cacheSolve to cache the inverse of that matrix. The values
## of the original matrix and its inverse are stored in this returned object,
## and can be accessed calling its functions get() and getInverse()
## respectively.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function receives an object returned by function makeCacheMatrix and
## returns the inverse of its original matrix. It tries to use a cached value,
## if available. If there's still no cached value, it calculates it and caches
## it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
