##             Programming Assignment 2: Lexical Scoping

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. Below you can find a pair of functions that cache the 
## inverse of a matrix and assumesthat the matrix supplied is invertible.


##  1).   This first function "makeCacheMatrix" creates a special "matrix" 
## object that can cache its inverse.What the function must do, is to

## Set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
  }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


##    2).    The following function "cacheSolve" computes the inverse of the 
## special matrix returned by makeCacheMatrix above. But first checks if the
## inverse has already been calculated.  If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets its value in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

