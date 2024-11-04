## Put comments here that give an overall description of what your
## functions do

## The first function takes a matrix as an argument and creates a special
## matrix object which allows us to cache its inverse using different get and
## set methods.

makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function (y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) inversion <<- invers
  getinvers <- function() inversion
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## The second function takes the special matrix as an argument with additional
## unspecified arguments. The function checks whether the matrix object has
## its inverse cached already. If there is a cached value - it is returned,
## otherwise the function calculates the inverse of the matrix, caches it into
## the object and returns the result of the inversion.

cacheSolve <- function(x, ...) {
  inversion <- x$getinvers()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setinvers(inversion)
  inversion
}