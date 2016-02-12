## makeCacheMatrix creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
    inv <<- inverse
  getInverse <- function()
    inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve creates the inverse of the matrix that was created by the function
## "makeCacheMatrix", and if the inverse has already been created, this function
## will retrieve it and not calculate again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
