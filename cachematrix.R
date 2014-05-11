## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     xInv<-NULL
     set<-function(y) {
          x <<- y
          xInv <<- NULL
     }
     get <- function() {
          x
     }
     setInv <- function(inverse) {
          xInv <<- inverse
     } 
     getInv <- function() {
          xInv
     }
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     xInv <- x$getInv()
     if(!is.null(xInv)) {
          message("retrieving cached inverse")
          return(xInv)
     }
     matrixData <- x$get()
     xInv <- solve(matrixData, ...)
     x$setInv(xInv)
     xInv
     
        ## Return a matrix that is the inverse of 'x'
}
