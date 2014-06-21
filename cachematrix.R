## This function creat a special "matrix" containing a list of 4 functions
## for solving and caching the Inverse matrix of the origin

## This function creat a matrix with 4 functions: set, get, setInverse and get Inverse.
## The actual solving and caching procession is in next function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) i <<- Inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function get an "matrix with 4 functions", and try to use the cached inverse matrix
## If cached data does not exist, it will solve it and cache it in 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
