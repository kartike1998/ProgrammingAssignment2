## The "cache matrix" functions implement caching the inverse of a matrix for
## faster retrieval using the lexical scoping rules in R


## 'makeCacheMatrix' provides methods to create a cache and use it to store a
## matrix and its inverse. Can initialize an empty matrix or accept a matrix
## object if provided through arguments. Returns a list containing the object
## and the related methods.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(n) {
        m <<- n
        i <<- NULL
    }
    get <- function() m
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' searches the cache for the inverse of the matrix provided. If
## available, the function retrieves the inverse, else computes the inverse and
## stores it in the cache, using methods defined in 'makeCacheMatrix'. Returns
## the inverse.

cacheSolve <- function(m, ...) {
    i <- m$getinv()
    if(!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    mat <- m$get()
    i <- solve(mat, ...)
    m$setinv(i)
    i
}