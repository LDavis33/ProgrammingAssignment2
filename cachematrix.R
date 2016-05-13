## Caching the Inverse of a Matrix
## This function will cache the inverse of a matrix which eliminates the need for computing it repeatedly during any given project.
## This function enables anyone to create a invertible matrix that can be chached.

makeCacheMatrix <- function(x = matrix()) {
  MX <- NULL
  set <- function(y) {
    x <<- y
    MX <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) MX <<- inverse
  getInverse <- function() MX
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If not already done, this function computes the inverse of the matrix created by makeCacheMatrix. If it has already been calculated or changed, it will then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  MX <- x$getInverse()
  if (!is.null(MX)) {
    message("getting cached data")
    return(MX)
  }
  mDat <- x$get()
  MX <- solve(mDat, ...)
  x$setInverse(MX)
  MX
}
