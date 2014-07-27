##The makeCacheMatrix and cacheSolve functions work together to cache the inverse of a
##matrix to be recalled at a future time without having to be recalculated

## This function creates a matrix object that can cache its own inverse

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

##This function checks to see if the inverse of the matrix in question has already been
##calculated.  If so it returns the cached inverse. If not it calculates the inverse, saves
##it to cache and then returns the calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
