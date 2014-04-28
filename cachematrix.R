## Cached matrix representation
# usage:
# > x <- matrix(sample(1:1000,9,replace=T), nrow=3, ncol=3)
# Create object:
# > obj <- makeCacheMatrix(x)
# Get setted matrix:
# > obj$get()
# Calculate inverse of the matrix, if cached, return cached result
# > cacheSolve(cx)

## Return matrix with defined functions:
# set - set matrix
# get - get setted matrix
# setinv - set inverse of matrix
# getinv - get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  # store inverse of matrix
  inv <- NULL
  
  # set the matrix
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  
  # set inverse of the matrix
  getinv <- function() inv
  
  # return the matrix with defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
# if inverse is already solved, return cached value
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # check if inverse already solved
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # solve inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache result
  x$setinv(inv)

  inv
}
