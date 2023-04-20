# Define a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize a cache to store the inverse matrix
  cache <- NULL
  
  # Define a function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    # Clear the cache when a new matrix is set
    cache <<- NULL
  }
  
  # Define a function to get the matrix
  get <- function() x
  
  # Define a function to set and get the inverse of the matrix
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  
  # Return a list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Compute the inverse of a matrix and cache the result
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse.")
    return(inverse)
  }
  
  # If not, compute the inverse
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Cache the inverse
  x$setinverse(inverse)
  
  # Return the inverse
  inverse
}
