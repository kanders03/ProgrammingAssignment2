## The below functions will allow for caching the calculation of inverting a matrix provided by the user.
## Author: Kevin Anderson, Co-Author/Design: Roger Peng

## Creates an object that supports solution caching.
makeCacheMatrix <- function(x = matrix()) {

  local_matrix <- NULL
  
  ## Setter for the cache object
  set <- function(y) {
    x <<- y
    solution <<- NULL
  }
  
  ## Getter for the cache object
  get <- function() {
    x
  }
  
  ## Allows object's solution to be cached
  setinverse <- function(the_solution) {
    solution <<- the_solution
  }
  
  ## Getter for the solution
  getinverse <- function() {
    solution
  }
  
  ## Returns the object's functions.
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Solves and caches the cached matrix object provided
cacheSolve <- function(x, ...) {
  
  ## Get the current (cached) solution and return if not null.
  sol <- x$getinverse()
  if (!is.null(sol)) {
    message("Getting Cached Data")
    return(sol)
  }
  
  message("Solution was not cached, calculating");
  ## Get and solve the matrix
  data <- x$get()
  sol <- solve(data)
  x$setinverse(sol)
  
  return (sol)
}


## Example for testing
testMyObject <- function()
{
  test_matrix <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, byrow=TRUE)
  
  cache_matrix <- makeCacheMatrix()
  cache_matrix$set(test_matrix)
  
  valid_solution <- matrix(c(-24,20,-5,18,-15,4,5,-4,1), nrow=3);
  
  cacheSolve(cache_matrix)
  
  validate <- cache_matrix$getinverse()
  
  message("The solution is: ");
  print(cache_matrix$getinverse())
  message("The solution should be: ")
  print(valid_solution)
  
}