## These functions create a special "matrix" that can cache its inverse and allow the 
## cached inverse to be retrieved from from the cache.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
            }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cachSolve either computes the inverse of the special "matrix" or, if it has already
## been computed, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
