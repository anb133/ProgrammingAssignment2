## Functions (makeCacheMatrix and cacheSolve) allow not to 
## compute matrix inversion repeteadly, but rather cache the
## inverse of the matrix. 

## Function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse, The "matrix" object
## is really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse matrix
## (4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(z){
    x<<- z
    m<<- NULL
  }
  get<- function() x
  setinverse<- function(inv) m<- inv
  getinverse<- function() m
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  n<- x$get()
  m<- solve(n, ...)
  x$setinverse(m)
  m
}
