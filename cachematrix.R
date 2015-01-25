## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##  cacheSolve: 
## This function computes the 
##     1) Inverse of the special "matrix" returned by makeCacheMatrix above. 
##     2) If the inverse has already been calculated (and the matrix has not changed), 
##          then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
   ## solve() returns the inverse of matrix
  m <- solve(data)
  x$setinverse(m)
  m
}
## example
## x <- matrix(c(-2,-1,1,2),2,2)
## m.c <- makeCacheMatrix(x)
## m.c$get()
##      [,1] [,2]
## [1,]  -2    1
## [2,]  -1    2
## cacheSolve(m.c)
## [,1]      [,2]
## [1,] -0.6666667 0.3333333
## [2,] -0.3333333 0.6666667
## m.c$getinverse()
## [,1]      [,2]
## [1,] -0.6666667 0.3333333
## [2,] -0.3333333 0.6666667
## Retrieving from the cache in the second run
## cacheSolve(m.c)
## getting cached data.
## [,1]      [,2]
## [1,] -0.6666667 0.3333333
## [2,] -0.3333333 0.6666667

