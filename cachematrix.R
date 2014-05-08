## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## This function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:
##      1. set the value of the vector
##      2. get the value of the vector
##      3. set the value of the mean
##      4. get the value of the mean 
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
## Otherwise, it calculates the inverse matrix of the data and sets the value of the  
## in the cache via the setmean function.
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
