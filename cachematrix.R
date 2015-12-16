## The below two functions, makeCacheMatrix and cacheSolve, are useful for creating
## a list that allows users to get, set, and solve for the inverse of a matrix.


## makeCacheMatrix take a matrix and caches the inverse of the matrix.
## 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(solve) {
    i <<- solve
  }
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cachesolve takes an object created by the makeCacheMatrix function and
## retrieves its matrix, solves for the inverse, and then stores the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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





