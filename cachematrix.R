## Matrix inversion can often be a costly computation so some benefit
## may be dervied by caching the matrix inverse rather than computing
## it every time. The code below shows one inmplementation whereby
## matrix inversions are cached.


## This function creates a 'special' matrix that, in reality, returns
## a list.
##
## The return list contains functions to
##
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix inverse
##   4. get the value of the matrix inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix.
##
## If the inverse of the matrix has already been determined, then the
## inverse is read from cache. Otherwise, the inverse is computed and
## placed  in the cache for future reference.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    inv
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
