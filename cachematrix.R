## A pair of functions that compute and cache the inverse of a matrix.

## Make a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Modification of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        # return our "matrix"
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        # Search in cache at first
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If cache is empty
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
