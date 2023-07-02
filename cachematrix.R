# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    # Reset the inverse cache when the matrix is updated
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse matrix
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  # Function to get the inverse matrix
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(cacheMatrix) {
  # Get the matrix from the cache
  x <- cacheMatrix$get()
  
  # Check if the inverse is already computed and cached
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # Compute the inverse
  inverse <- solve(x)
  
  # Cache the inverse
  cacheMatrix$setInverse(inverse)
  
  inverse
}

#how to use it 
mat <- matrix(c(1, 2, 3, 4), nrow = 2)

# Create a cache matrix object
cacheMat <- makeCacheMatrix(mat)

# Compute the inverse
cacheSolve(cacheMat)  # Calculates the inverse

# Get the inverse from the cache
cacheSolve(cacheMat)
