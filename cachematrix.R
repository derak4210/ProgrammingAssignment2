## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y) {
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


## this function computes the inverse of the special "matrix" like object,
## if the inverse has already been calculated (and matrix hasn't changed)
## the function will return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ... )
  x$setinverse(m)
  m
}

