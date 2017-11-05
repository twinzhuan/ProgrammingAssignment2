## Functions to calculate inverse of matrix which make use of caching

## Creates a list of four elements which sets, gets the matrix for which
## we'd like to calculate the inverse of, and also get and set the cached
## calculation result

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes as the first argument the list produced by the
## above function. It returns the inverse of the matrix stored in the
## list via a call through the set function. Example:
##
## m = matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3))
## c = makeCacheMatrix()
## c$set(m)
## cacheSolve(c)
## > (first call returns the resulting invese matrix)
## cacheSolve(c)
## > (second call retuns the same matrix but this times from the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
