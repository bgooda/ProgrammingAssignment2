## The following two functions are used to calculate the inverse of a matrix,
## and cache it for future use.
## To use the caching functions, do the following steps:
## 1- First, call the function makeCacheMatrix() with your input matrix,
##    and save the returned object to a variable.
## 2- Second, call the function cacheSolve() with the returned object.
##    If this is your first call with that matrix, the inverse will be computed.
##    If you call this function again with the same matrix, you will get the cached inverse.


## The following function creates the Cached Matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The following function uses the cached matrix object, to get, or set the matrix inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
