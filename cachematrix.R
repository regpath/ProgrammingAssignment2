## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse
  setinv <- function(solve) m <<- solve
  
  # Get the value of the inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Get the inverse of the matrix
  m <- x$getinv()
  
  # Check if there is any value in 'm'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix and store it in 'data'
  data <- x$get()
  
  # Get the inverse matrix of it
  m <- solve(data, ...)
  
  # Set it to the inverse of it.
  x$setinv(m)
  m
}
