## This file containing two functions of R that stores a matrix cache's its inverse.

## The first function creates a list containing four function to set the value of the
## matrix, get the value of the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(invers) inv <<- invers
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The second function calculates the inverse of the special matrix created by the
## above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$set(inv)
      inv
}
