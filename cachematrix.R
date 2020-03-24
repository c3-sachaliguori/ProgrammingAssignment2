## Put comments here that give an overall description of what your
## functions do

## Creates a new matrix object that can cache inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  ## set the matrix field
  set <- function(mat) {
    m <<- mat
    i <<- NULL
  }

  ## get the matrix field
  get <- function() {
    m
  }

  ## Set the inverse field
  setInverse <- function(inverse) {
    i <<- inverse
  }

  ## Return the inverse field
  getInverse <- function() {
    i
  }

  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the  matrix returned by makeCacheMatrix.
## If the inverse is already calculated, and the matrix remains the same,
## function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get cached matrix
  m <- x$getInverse()

  ## Return the inverse if its non null
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()

  ## Calculate the inverse
  m <- solve(data) %*% data

  ## set the inverse
  x$setInverse(m)

  ## Return the matrix
  m
}
