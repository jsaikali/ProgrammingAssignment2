## These functions together will allow you to get & set
## an invertible matrix's value and inverse using a list 
## so that the inverse of the original matrix only needs 
## to be computed once.


## makeCacheMatrix will take in a matrix and will
## create list containing functions to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse matrix
## (4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a list returned by makeCacheMatrix 
## and either:
## (1) calculates the inverse matrix and stores it in that list.
## (2) retrieves the already-stored inverse matrix from that list.
## The inverse matrix is then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
