## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Following ufnction creates a speacial matrix object that can cahe its inverse matrix

#Creating a function with an empty matrix.
makeCacheMatrix <- function(mat = matrix()) {
  # Initializing a inverse matrix to NULL
  inverse   <- NULL
  set <- function(x) {
    mat <<- x
    inverse  <<- NULL
  }
  get <- function() mat
  setinverse <- function(inv) inverse  <<- inv
  getinverse <- function() inverse 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function

#Following function computes inverse of above matrix (the special matrix)
# function saves the calculated values in matrix
cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- mat$getinverse()
  # Check for NuLL values in inverse matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinverse(inverse)
  inverse
}

