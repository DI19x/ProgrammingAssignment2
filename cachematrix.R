### Week 3 - Programming Assignment

## 2 functions (makeCacheMatrix and cacheSolve):

## makeCacheMatrix
# Creates a matrix; this is really a list with 4 functions (set value of matrix,
# get value of matrix, set value of inverse and get value of inverse)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve
# Computes inverse of the matrix returned by makeCacheMatrix
# If inverse already calculated (+ matrix unchanged), retrieves inverse from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
