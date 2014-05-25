## The following functions calculate and cache the
## inverse of a matrix.

## This first function, makeCacheMatrix, creates a special
## matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(0)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function, cacheSolve, computes the inverse
## of the special matrix returned by makeCacheMatrix
## above or, if already calculated, retrieves it
## from the cache.

cacheSolve <- function(x, ...) {
        ## checks first to see if inverse is already
        ## in cache
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## calcs the (uncached) inverse if necessary
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}