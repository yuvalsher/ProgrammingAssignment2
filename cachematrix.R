## Put comments here that give an overall description of what your
## functions do
## This file contains two functions that implement a matrix object for which
## the computed inverse matrix can be cached.

## Write a short comment describing this function
## The makeCacheMatrix function implements a matrix object that provides caching
## of a related value, the inverted matrix in the exercise.
makeCacheMatrix <- function(x = matrix()) {

    ## When creating a new matrix object, set the cached value to NULL.
    cache <- NULL
    
    ## The set() function sets the matrix value, and resets the cached value.
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    ## The get() function returned the matrix itself.
    get <- function() x
    
    ## The setcache() function sets the cached value.
    setcache <- function(new_cache) cache <<- new_cache
    
    ## The getcache() function returns the cached value.
    getcache <- function() cache
    
    ## The returned object of this function is a list of the above four functions
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of a matrix that was created 
## using the makeCacheMatrix function. If this function was already called
## for the matrix 'x', the cached value of the inverse will be used.
cacheSolve <- function(x, ...) {
    ## Check if x has a cached value.
    m <- x$getcache()
    if(!is.null(m)) {
        ## Yes - just return the cached value
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix itself
    data <- x$get()
    ## Compute the inverse matrix.
    m <- solve(data, ...)
    ## Cache it.
    x$setcache(m)
    ## And return the inverted matrix.
    m
    
}
