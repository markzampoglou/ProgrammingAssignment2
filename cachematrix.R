## Store a matrix and cache its inverse in a single object.

## All functions, even single-line ones, are defined using "{ }" for 
## structure and clarity


# Function can be tested by calling "x<-makeCacheMatrix(matrix(rnorm(25),5,5))"
# and then "x$get()"

makeCacheMatrix <- function(x = matrix()) {
     xInv<-NULL
     ## set: define matrix, reinitialize the inverse to NULL
     set<-function(y) {
          x <<- y
          xInv <<- NULL
     }
     ## get: return the value of the matrix
     get <- function() {
          x
     }
     ## setInv: store the inverse (actually no inverse computation takes place here)
     setInv <- function(inverse) {
          xInv <<- inverse
     } 
     ## getInv: return the stored value of the inverse
     getInv <- function() {
          xInv
     }
     # Return a list of all functions
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculates the matrix inverse, if not already cached in memory. If cached,
## retrieves the stored inverse

## Test by calling "cacheSolve(x)" (after having defined x). Validate accuracy
## of results by running "round(x$get() %*% cacheSolve(x),10)"

cacheSolve <- function(x, ...) {
     xInv <- x$getInv()
     if(!is.null(xInv)) {
          message("retrieving cached inverse")
          return(xInv)
     }
     matrixData <- x$get()
     xInv <- solve(matrixData, ...)
     x$setInv(xInv)
     ## Return a matrix that is the inverse of 'x'
     xInv
}
