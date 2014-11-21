## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions use a cache solution to avoid calculating the inverse of the 
## same matrix more than once

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Atribute
  m <- NULL
  
  ## Matrix setter
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Matrix setter
  get <- function() x
  
  ## Invertion setter
  setinver <- function(inver) m <<- inver
  
  ## Invertion getter
  getinver <- function() m
  
  ## List of functions available
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinver()
  
  ## If we have the result in our cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Only if we don't have the result yet
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
