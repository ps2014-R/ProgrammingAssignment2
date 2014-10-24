
## makeCacheMatrix "wraps" a matrix in a structure capable of caching the matrix inverse.
## The resulting structure can be passed to cacheSolve to either compute the matrix inverse,
## or return the cached inverse if it has previously been computed.


## makeCacheMatrix creates a list of functions for manipulating a matrix, passed
## as the function argument. Typically, a matrix is passed to makeCacheMatrix, and
## the resulting structure is passed to cacheSolve to get the matrix inverse.
## The matrix inverse is cached by setting it in the parent environment of the
## setInverse() function, that is, in the environment of the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {

        cachedInverse <- NULL   # cached matrix inverse value
        set <- function(y) {    # set a new matrix, remembering to nullify cached inverse
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x     # get matrix
        # Set inverse in parent environment, i.e. in environment of makeCacheMatrix
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve returns the inverse of a matrix passed to the makeCacheMatrix
## function. The result of that function is passed to cacheSolve and the
## matrix inverse is either computed if this is the first time cacheSolve has
## been called on that matrix, or a cached matrix inverse is returned.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        cachedInverse <- x$getInverse()
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                return(cachedInverse)
        }
        data <- x$get()
        cachedInverse <- solve(data, ...)
        x$setInverse(cachedInverse)
        cachedInverse
}

