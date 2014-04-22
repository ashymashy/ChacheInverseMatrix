

## The first function, makeCacheMatrix creates a special "matrix"
## set the value of the matrix
## get the value of the matrix
## set the value of the solve for inverse
## get the value of the solve for inverse

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

## However, it first checks to see if the solve has already been calculated.
## If so, it gets the solve from the cache.
## Otherwise, it calculates the solve of the data and sets the value
## of the solve in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
