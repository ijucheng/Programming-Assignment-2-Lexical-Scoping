# makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(x1) {
    x <<- x1
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function(inve1) inve <<- inve1
  getInverse <- function() inve
  list(set = set,  ## the value of a matrix
       get = get,  ## get the value of a matrix
       setInverse = setInverse,  ## get the cahced value (inverse of the matrix)
       getInverse = getInverse  ## get the cahced value (inverse of the matrix)
)
}


# cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x1, ...) {
  inve <- x1$getInverse()  ## get inverse x1
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  m <- x1$get()
  inve <- solve(m, ...)
  x1$setInverse(inve)
  inve
}
