## The following are two functions that allow for the computation of matrix 
## inverses, drawing data either from computation or from the cache in order to 
## complete the process most efficiently. 

## The first function creates the cache of data in the form of a list of
## functions through several different steps. First, it stores
## the input data, the matrix (x), in the cache list as the variable "set".
## It also names the variable "m" as the value of NULL. Then it stores the value
## of that function in "get". Then it creates a function establishing the
## inverse of the matrix as "setinvert" and finally it gets the inverse through
## "getinvert". All of these functions are stored in a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinvert <- function(Solve) {m <<- Solve}
  getinvert <- function() {m}
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function takes the input matrix and runs it through the list of cached 
## data. If it has already been computed in the past, the stored data will be 
## pulled from the above cache by calling the getinvert() function and a message
## "getting cached data". If m is NULL, meaning no cached data exists for the
## matrix, the matrix will be run through the solve function using "get" and
## "setinvert".

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}