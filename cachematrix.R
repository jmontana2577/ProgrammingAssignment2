## The following functions return the inverse of a matrix.
## If the inverse exists in cache, it is retrieved, otherwise
## it is calculated.

## This function sets the value of the matrix, gets the value
## of the matrix, sets the value of the inverse, and gets
## the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks cache for the inverse. If exists, 
## it returns the inverse. If not, it calculates the inverse,
## writes it to cache, and returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("Getting inverse from cache.")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}
