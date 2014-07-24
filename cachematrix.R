## Two functions that cache the inverse of a matrix
## Coursera "rprog-005" Assignment 2

## makeCacheMatrix takes a matrix as argument
## and creates a list of functions in order to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Returns a list containing functions to:
  ## set: set the matrix x
  ## get: get the matrix m
  ## setInv: set the inverse matrix x_inv
  ## getInv: get the inverse matrix x_inv
  x_inv <<- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) x_inv <<- inv
  getInv <- function() x_inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve takes the return list from makeCacheMatrix as argument
## and either computes or retrieves the cached inverse matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverted <- x$getInv()
  if(!is.null(inverted)) {
    message("getting cached inverse matrix")
    return(inverted)
  }
  x_mat <- x$get()
  inverted <- solve(x_mat,...)
  x$setInv(inverted)
  inverted
}
