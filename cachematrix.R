
## makeCacheMatrix takes a matrix which is invertible - http://mathworld.wolfram.com/MatrixInverse.html
## produces a list representation for consumption by function cacheSolve which either returns a matrix
## inverse if already precalculated and cached, or generates a new inverse and caches that matrix.
##
## Note:internal functions are not directly callable intended for use with cacheSolve
##
##  example usage 
##
##
##    b = matrix (c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)
##    b
##          [,1] [,2] [,3]
##    [1,]    1    3    3
##    [2,]    1    4    3
##    [3,]    1    3    4
##
##    matrixB = makeCacheMatrix(b)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve takes a makeCacheMatrix list representation of matrix generated in function
## makeCacheMatrix. An inverse matrix is returned for an invertible matrix only.
##
## example usage
##
##    inverseB <- cacheSolve(matrixB)
##    inverseB
##          [,1] [,2] [,3]
##    [1,]    7   -3   -3
##    [2,]   -1    1    0
##    [3,]   -1    0    1
##
##  subsequent calls of cacheSolve(matrixB) will read cached data and report "getting cached data"
## to the console.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
