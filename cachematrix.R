# Calculate inverse for square matrix, and cache the result

# source("cachematrix.R")
# > rr <- makeCacheMatrix(matrix(rnorm(25), 5, 5))
# > cacheSolve(rr)
#
# Clear cached value
# > rr$setinv(NULL)


# Set/get values for a matrix
# Set/get values for inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#Get inverse, if doesnt exists, calculate inverse and cached
cacheSolve <- function(x, ...) {
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