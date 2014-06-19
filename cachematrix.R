## My functions calculate the inverse of a "special" matrix created in function 1 (makeCacheMatrix), stores
##it in memory  and retrieve it from cache
## To avoid making lengthy computations over and over again and save some time.

## This function works exactly as the example function but for a given matrix and it's inverse, what it does is
##create a list containing functions to set and retrieve the value of the matix and it's inverse.
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
## starts making computations to calalculate the inverse, the program first
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
