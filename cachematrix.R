## Objective is to create two functions to cache the inverse of a matrix
## assuming that the matrix supplied is always invertible

## 1.The first function makeCacheMatric creates a special "matrix" type object
## that will be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setmatrix <- function(matinverse) cm <<- matinverse
  getmatrix <- function() cm
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## 2.The second function cacheSolve computes the inverse of the special matrix
## that is returned by the makeCacheMatrix function. If the inverse is already
## computed then cacheSolve retrieves the inverse from the cache, given that the 
## matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cm <- x$getmatrix()
  if(!is.null(cm)) {
    message("getting cached matrix...")
    return(cm)
  }
  else {
    datamat <- x$get()
# solve() is used to compute the inverse of the matrix
    cm <- solve(datamat, ...)
    x$setmatrix(cm)
    return(cm)
  }
}