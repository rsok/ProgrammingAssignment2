## this is a wrapper for original matrix
## and place for keeping the already calculated inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## this function performs real operation, but only if needed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtx <- x$getsolve()
        if (!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
        }
        data <- x$get()
        mtx <- solve(data, ...)
        x$setsolve(mtx)
        mtx
}
