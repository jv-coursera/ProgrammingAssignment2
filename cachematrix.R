## The following functions compute the inverse of a matrix and
## cache the result. 


## This function creates the "matrix" object and cache.
makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse as Null.
  i <- NULL
  #set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get matrix
  get <- function() x
  #set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  #get the inverse of the matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of an "matrix" object
## created by makeCacheMatrix.It will retrieve the matrix from
## the cache if it has been computed before.
cacheSolve <- function(x, ...) {
  #get the inverse from the cache if it's already been computed
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #compute and cache the inverse if it has not been cached
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
