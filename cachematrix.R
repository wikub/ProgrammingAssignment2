## Put comments here that give an overall description of what your
## functions do


## This function allows you to build an object containing a matrix 
## and the result of the inverse.
## There are internal functions that allow access to the matrix and the inverse of the matrix ("solve" results).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInvert <- function(invert) i <<- invert
    getInvert <- function() i
    list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}


## This function calculates the inverse of matrix if it is not cached in the input object.
## In case of calculation, the result is cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInvert()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInvert(i)
    i
}
