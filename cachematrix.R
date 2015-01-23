## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function (y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() { x }
  setSolve <- function (y) {
    inverseX <<- y 
  }
  getSolve <- function () {
    inverseX
  }
  list (set=set,get=get,
        setSolve=setSolve,
        getSolve=getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invX <- x$getSolve() 
  if (is.null(invX)) {
    invX <- solve(x$get())
    x$setSolve(invX)
  } else {
    message("getting cached data")
  }
  invX
}
