## A pair of fcns that inverts a matrix in a memory-efficient way.
## First, assign makeCacheMatrix(your_matrix) to a variable.
## Then apply cacheSolve to that new variable.

## This function creates a list of functions to better utilize R's cache.
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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setsolve(m)
    m
}
