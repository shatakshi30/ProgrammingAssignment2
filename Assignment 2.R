#Assignment 2

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the variable 'inv' to store the inverse of the matrix
  inv <- NULL
  
  # Define a function 'set' to set the value of the matrix 'x'
  set <- function(y) {
    x <<- y  # Assign the value of 'y' to 'x' using '<<-' to make it a global assignment
    inv <<- NULL  # Reset the 'inv' variable to NULL since the matrix has changed
  }
  
  # Define a function 'get' to retrieve the value of the matrix 'x'
  get <- function() x
  
  # Define a function 'setinverse' to set the value of the matrix inverse
  setinverse <- function(inverse) inv <<- inverse  # Store the inverse value in 'inv' using '<<-'
  
  # Define a function 'getinverse' to retrieve the value of the matrix inverse
  getinverse <- function() inv
  
  # Return a list of the defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
  # Retrieve the current value of the matrix inverse from the cache
  inv <- x$getinverse()
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  # If the inverse is not cached, calculate it
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)  # Calculate the inverse using 'solve' function
  
  # Cache the calculated inverse
  x$setinverse(inv)
  
  # Return the calculated inverse
  inv
}
