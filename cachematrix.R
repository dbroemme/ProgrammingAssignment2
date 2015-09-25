## Programming Assignment #2
## These functions deal with caching the inverse of a matrix.

## This functions creates a special matrix that 
## can cache it's own mean

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mi <<- inverse
    getinverse <- function() mi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will return the inverse of the given matrix.
## If it has already calculated the inverse, it will use
## the cached version, otherwise it will calcalate the inverse
## and then store the result in the mi variable for future use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    mi <- x$getinverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    matrixdata <- x$get()
    # Use solve to create the matrix inverse
    mi <- solve(matrixdata, ...)
    x$setinverse(mi)
    mi
}
