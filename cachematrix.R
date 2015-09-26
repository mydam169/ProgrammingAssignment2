##The first function creates a special `matrix` object to cache the inverse
##The second function does operations on matrix x: compute the inverse with the function
##`solve' if the computation has not been done yet, and retrieve the cached value if
##the matrix has already been inverted at an earlier command/computation

## `makeCacheMatrix` does the following:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##`cacheSolve`is used to either retrieve the inverse of a matrix if the operation
##has already been done, or computes it if it has not been.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
