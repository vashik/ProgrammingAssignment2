## This is a couple of functions which is used to cache
## inverse of a matrix

## Creates a special object to store inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## get & set original matrix
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  
  ## get & set the inverse of a matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Returns the inverse of matrix 'x'. 
## If the inverse is not present in cache, will calculate it and store to cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  
  inv
}
