## Two functions that implement calculating and caching 
## the inverse of a matrix

## Implements the caching. It takes a plain matrix
## and returns a list with different functions
## to operate on this matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the current matrix
  get <- function() x
  
  ## Set the inverse
  setInv <- function(inverse) inv <<- inverse
  
  ## Get the inverse
  getInv <- function() inv
  
  ## Return list of functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## Fetch the cached inverse of x if possible
  inv <- x$getInv()
  
  ## If inverse has been calculated already
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## Inverse empty. Calculate inverse and store it in x
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  
  ## Return inverse
  inv
}
