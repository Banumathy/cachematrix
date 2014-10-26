cachematrix
===========
makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  get <- function() z
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

cacheSolve <- function(z, ...) {
   inv <- z$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- z$get()
  inv <- solve(data, ...)
  z$setinverse(inv)
  inv
}
