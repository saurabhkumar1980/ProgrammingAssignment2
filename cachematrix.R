## Functions makeCacheMatrix and cacheSolve demonstrate how caching can help
## reduce the time it takes to fetch the same result. Once the result is cached,
## the result can be retreived from cache instead of recalculating it.

## makeCacheMatrix takes a matrix as an input, calculates the inverse of that matrix,
## stores the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes the matrix as an input, checks to see if the inverse of 
## the matrix is already available. If the inverse is already available in the 
## cache, it returns that. Otherwise, it calls setinverse to calculate the 
## inverse and store it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  ## Following if statement checks if inverse is stored in the cache. If available
  ## cached inverse is retreived and fuction ends with the return statement
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  ## If cached inverse is not available, following statements calculate and 
  ## store inverse in the cache
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  # Return inverse
  n  
}
