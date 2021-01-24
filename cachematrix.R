## Put comments here that give an overall description of what your
## functions do


## Function that takes a matrix as input. If a matrix is given, it stores 
## its value in the variable inv, otherwise it sets inv=NULL
## It returns a list of methods to: 
## 1:set the value of the matrix
## 2:get/retrieve the matrix
## 3:set its inverse, which will be used by the cachesolve function
## 4:get its inverse, which will be also used by cachesolve
## cache the value of the inverse of the matrix if it exist

makeCacheMatrix <- function(x = matrix()) {
  # Retruns a list of methods to apply to the makeCacheMatrix object
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## it calculates the inverse of a matrix. If the matrix exists previously
## in the memory, it retrieves it. Otherwise it calculates it and cache its 
## value for later use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
