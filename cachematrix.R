## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "Matrix", 
# which is really a list containing a function to
# set the value of the Matrix
# get the value of the Matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- solve(x)
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


## Write a short comment describing this function
# cacheSolve calculates the inverse of the special "Matrix" 
# created by makeCacheMatrix. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates Matrix inverse by setinv function.
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
