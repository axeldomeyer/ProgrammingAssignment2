##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinversion <- function(inversion) i <<- inversion
  getinversion <- function () i
  list (set=set, get=get, setinversion=setinversion, getinversion=getinversion)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinversion ()
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinversion(i)
  i
  
}
