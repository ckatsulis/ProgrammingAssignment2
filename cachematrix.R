## The purpose of these two functions is to create a class that will store and display the 
## value of a matrix and also calculate and cache the value of its inverse.  This will eliminate
## the costly calculation time when the action has already been taken.

## makeCacheMatrix
## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:
##      1. set the values of the matrix
##      2. get the values of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## The following function calculates the inverse matrix of the special "matrix" created with the function makeCacheMatrix. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
