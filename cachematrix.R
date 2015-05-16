#
# Function: makeCacheMatrix
#
# This function creates a special "matrix" object - actually a list of set and get 
# functions - that can cache its inverse.
#
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  setMat <- function(y) {
    m <- y
    i <- NULL
  }
  getMat <- function() m
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(setMat = setMat,
       getMat = getMat,
       setInv = setInv,
       getInv = getInv)
  
}

#
# Function: cacheSolve
#
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
#
cacheSolve <- function(f, ...) {
  # Checking the cache:
  i <- f$getInv()
  # If found (not null), return the cached inversed matrix.
  if(!is.null(i)) {
    message("I'm getting cached data")
    return(i)
  }
  # Getting the matrix to be inversed.
  data <- f$getMat()
  i <- solve(data)
  # Cacheing the result.
  f$setInv(i)
  # Return value.
  i
}
