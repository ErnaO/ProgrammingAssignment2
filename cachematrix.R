## Matrix inversions might be computational differnt, especially with
## large matrices. These functions will cache the inverse when it has been 
## computed. Every time it will check beforehand if the matrix
## has been inverted before. If so, the inverse will be caught from cachhe.

## makeCacheMatrix creates a special object that stores a matrix and caches
## its inverse
## The value of the matrix is set and can be gotten
## The value of the inversie is set and can bet gotten

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInvervse
         getInverse = getInverse)

}


## cacheSolve computes the inverse of special "matrix" from makeCacheMatrix. When it was 
## calculated before, it will get the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        ## Matrix was inverted before and result will be gotten from cache
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Matrix was not inverted before, computation will be done now
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
