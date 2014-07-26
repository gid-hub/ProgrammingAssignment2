## The following functions form an example of how to cache the
## result of an expensive operation.


## Store a matrix and create a list containing functions to get/set 
## the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {

  # The cached inverse
  inv <- NULL

  # Setters & getters
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv

  # Return the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the inverse of the matrix stored by 'x', calculating it
## only when we haven't done so before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Get the cached inverse and return it if possible
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Calculate the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)

  # Return the inverse
  inv
}
