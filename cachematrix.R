## makeCacheMatrix, cacheSolve
## These two functions can be used to create a special object that stores 
## a square matrix and caches its inverse so that the inverse does not have 
## to be recalculated if the matrix has not changed.

## 1. makeCacheMatrix 
## creates a list containing functions to:
## - set the value of the matrix, 
## - get the value of the matrix
## - set the inverse matrix
## - get the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
    mI <- NULL
    set <- function(y) {
        m <<- y
        mI <<- NULL
    }
    get <- function() m
    setmI <- function(matrixInv) mI <<- matrixInv
    getmI <- function() mI
    list(set = set, get = get,
         setmI = setmI,
         getmI = getmI)
}


## 2. cacheSolve
## returns the matrix inverse either from the cache or caclulates
## it if the cache is empty.
## NOTE: the matrix passed to cacheSolve must be square

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
    mI <- m$getmI()
    if(!is.null(mI)) {
        message("getting cached data")
        return(mI)
    }
    data <- m$get()
    mI <- solve(data, ...)
    m$setmI(mI)
    mI
}
