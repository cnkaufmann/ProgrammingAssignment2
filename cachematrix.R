## The purpose of these two functions (makeCacheMatrix and cacheSolve) are to do computations
## to create an inverse matrix, but instead of creating this inverse matrix every time, it
## would store a result in a cache in another environment and retrieve that if it had been
## previously calculated.

## makeCacheMatrix() serves as a list of functions that can get and set a matrix and its inverse
## Note that this does not do any calculations. It is simply something taht gets and sets
## the matrixs.

makeCacheMatrix <- function(x = matrix()) {
  #Creating a null matrix:
  m <- NULL
  #Creating a function that takes a matrix object and stores it in a chached matrix object
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  #Creating a function that gets cached matrix created above
  get <-function() {
    x
  }
  #Creating a function that stores inverse of that matrix in cache (which will be created later)
  setInv <- function(Inverse) {
    m <<- Inverse
  }
  #Creating a function that gets the inverse of that matrix from the cache
  getInv <- function() {
    m
  }
  #Generating a special list of all these items:
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve() processes a matrix to turn it into an inverse matrix using the processing
## functions created in makeCacheMatrix().

cacheSolve <- function(x, ...) {
  #Getting inversed matrix from cache:
  m <- x$getInv()
  #Checking to see if inversed matrix in cache exist, and if it does, retrieve it:
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Putting not inversed matrix into data object for processing
  data <- x$get()
  #Creating inversed matrix:
  m <- solve(data, ...)
  #Move this inversed matrix to cache
  x$setInv(m)
  #Return Inversed Matrix:
  m
}