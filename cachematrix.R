## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    list(setMat = setMat,
         getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(is.null(inv)) {
        # No value cached - calculate it
        inv <- TRUE
        x$setInv(inv)
    } else {
        message("Using cache data for matrix inverse")
    }
      
    inv
    ## Return a matrix that is the inverse of 'x'
}
