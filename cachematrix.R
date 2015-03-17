## Speed up calculating the inverse of a matrix by caching already calculated values.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) cachedInverse <<- solve
    getSolve <- function() cachedInverse
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    solve <- x$getSolve()
    if(!is.null(solve)) {
        message("getting cached data")
        return(solve)
    }
    data <- x$get()
    solve <- solve(data, ...)
    x$setSolve(solve)
    solve
}

## To test: 
## y<-diag(10000)
## makeCacheMatrix(y)
## system.time(solve(y))
## system.time(solve(y))
