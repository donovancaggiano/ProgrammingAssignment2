## My functions calculate the inverse of a "special" matrix created in function 1, stores it and retrieve it from cache
## To avoid making lengthy computations over and over again

## This function works exactly as the example function but for a given matrix and it's inverse
makeCacheMatrix <- function(x=matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s<<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of the "special" matrix created with the first function, but before that it
##checks if the inverse has been calculated already so it would just retrieve it from cache, thus skipping computation.
cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
