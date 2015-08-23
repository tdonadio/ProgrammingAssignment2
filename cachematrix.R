##
## File: cachematrix.R 
## Author: Tony Donadio
##
## Description: cachematrix.R is a solution to Programming
##   Assignment 2 for the Coursera R Programming class. It
##   provides functions that allow the creation and use of
##   a special matrix object that can cache its inverse to
##   improve performance.  
##
## makeCacheMatrix takes a matrix as input and returns a
##   list object consisting of the functions required to
##   create and access the matrix and its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    #
    # The variable "inv" contains the matrix inverse. On
    #   creation of the matrix object, set it to NULL so
    #   for "lazy evaluation" (only calculating its value
    #   when and if it's used for the first time).
    #
    inv = NULL
    verbose = FALSE
    #
    # The setVerbose function sets a flag to determine 
    #  whether text messages are written to the console.
    #
    setVerbose <- function(flag) {
        verbose <<- flag
    }
    #    
    # The set function sets the matrix variable "x" to a
    #  new value. It also sets inv to NULL, indicating
    #  that the next time the inverse is requested, that
    #  it will need to be re-calculated. These assignments
    #  are done with the "superassignment" operator <<-,
    #  which sets these values in the environment of the
    #  enclosing function (makeCacheMatrix).
    #
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    #
    # The function "get" simply returns the matrix value.
    # 
    get <- function() {
        if (verbose) message("Getting the matrix")
        x
    }
    # 
    # cacheSolve serves as the "setter" for the matrix inverse
    #  (there is no "setInv"). All the required calculations for
    #  calculating and setting the inverse are peformed here. The
    #  external "cacheSolve" function simply calls this method on
    #  the cached matrix object.
    #
    cacheSolve <- function(...) {
        #
        # If the inverse is NULL, then it hasn't been set yet.
        #   Calculate and set it. Pass additional parameters
        #   to solve using the ellipsis (...). Generate text
        #   messages if the "verbose" flag is set.
        #
        if (is.null(inv)) {
            if (verbose) message("Generating the inverse")
            inv <<- solve(x, ...)            
        }
        # 
        else {
            if (verbose) message("Getting cached inverse")
        }
        # Return the inverse
        inv
    }
    
    # getInv simply returns the inverse, *without checking*
    #  whether or not it needs to be evaluated first. As a
    #  result, it may return NULL if it is called before the
    #  first call to cacheSolve().
    #
    getInv <- function() inv
    # 
    # Return a list containing the cacheMatrix functions. 
    #
    list(set = set, get = get, cacheSolve = cacheSolve,
         getInv = getInv, setVerbose = setVerbose)
}

## cacheSolve is a function that returns the cached inverse
##   of a matrix created by makeCacheMatrix(), calculating
##   and caching it if it's being used for the first time.
##   All this function does is call the cacheSolve "setter"
##    method in the cached matrix object.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x$cacheSolve(...)
}