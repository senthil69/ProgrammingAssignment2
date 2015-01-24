## makeCacheMatrix - Creates a matrix that can cache the inverse. 
## cacheSolve - equivalent to solve() in core language. the inverse of matrix 
##            - computed only once and cached.
## Usage : mat - Create your own matrix of DIM N X N
##         m <- makeCacheMatrix(mat)
##         cacheSolve(m) - inverse is computed once
##         cacheSolve(m) - Returns cached inverse
##         m$set (matrix nrow=3,ncol=3, c(1,rep(0,3),1,rep(0,3),1)) 
##             -- Cached value is invalidated
##         cacheSolve(m) - inverse is computed again

## makeCacheMatrix(x - inputMatrix)
## Creates new matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  # Matrix can be changed
  set <- function (y) {
    x <<- y
    inverseX <<- NULL  #invalidate the cache
  }
  get <- function() { x }
  
  #Store the new cache value
  setSolve <- function (y) {
    inverseX <<- y 
  }
  
  #Get the cached inverse matrix
  getSolve <- function () {
    inverseX
  }
  list (set=set,get=get,
        setSolve=setSolve,
        getSolve=getSolve)
}


## Calculates inverse of matrix using solve() method. 
## Pass all the arguments necessary for solve() 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Retreive the inverse from cache
  invX <- x$getSolve() 
  if (is.null(invX)) {
    # Inverse is not in the cache, compute & set the value
    invX <- solve(x$get(),...)
    x$setSolve(invX)
  } else {
    # return the cached value
    message("getting cached data")
  }
  #return the inverse of the matrix
  invX
}
