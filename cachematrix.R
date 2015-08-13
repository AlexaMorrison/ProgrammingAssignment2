## The following functions cache the inverse of a matrix so it does not need to be calculated
## repeatedly
## For example, set up a square matrix
##1  matrix<-matrix(data=c(4,2,7,6),nrow=2, ncol=2)
##2  a<-makeCacheMatrix(matrix)    creates the 'cacheing' matrix
##3  b<-cacheSolve(a)              creates the inverse matrix of 'matrix'
##4  c<-cacheSolve(a)              retrieves the cached inverse matrix of 'matrix'
##                                  (i.e. identical(c,b)=true)
##   getting cached data           comment output by the function when retrieving from the cache

## This function creates a special "matrix" object that can cache its inverse
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


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
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
