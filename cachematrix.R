## Author:   dkolbe
## File:     cachematrix.R
## Purpose:  a container object that holds a matrix and can store a cached value
##           for the matrix inverse

## Function: makeCacheMatrix
## Purpose:  a container object for a matrix with getMat() and setMat() funcs
##           Also provides getInv() and setInv() to access a stored inv value
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y) {
        # Initialize with data for x and no value for inverse
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x # Return matrix
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    list(setMat = setMat,
         getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}

## Function: cacheSolve
## Purpose:  return the inverse of a matrix.  The matrix should be stored in
##           an object of the type returned by makeCacheMatrix.  If the inverse
##           has previously been calculated, a stored value is used.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(is.null(inv)) {
        # No value cached - calculate it
        inv <- solve(x$getMat())
        x$setInv(inv)
    } else {
        message("Using cache data for matrix inverse")
    }
      
    inv
    ## Return a matrix that is the inverse of 'x'
}
