## This function creates a special "matrix" object that can cache its inverse.
## This function creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve retrieve the inverse from 
# the cache. This function assumes that the matrix supplied is always invertible.
# The built-in function of R, solve(), is used for calculating the inverse of a matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv ## Returns a matrix that is the inverse of 'x'
}
