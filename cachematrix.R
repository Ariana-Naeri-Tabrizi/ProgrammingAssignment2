## These functions create a special matrix object that can cache
## its inverse. If the inverse has already been calculated, the
## cached value is returned instead of recalculating it.

## This function creates a special matrix object with functions to
## set/get the matrix and set/get its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  getinverse <- function() {
    inverse
  }
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## This function returns the inverse of the special matrix object.
## If the inverse is already cached, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}