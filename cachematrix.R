## This file is composed of 2 functions which aim to calculate the inverse of a matrix by catching.
## makeCacheMatrix is used in order to load the matrix to be inverted
## cacheSolve returns the value of matrix loaded with previous function

## makeCacheMatrix(x) returns a list of functions that will be the arguments of cacheSolve in order to calculate the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix inserted had been calculated, if yes returns catched value if not calculates, catches and returns it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
  inv
}
