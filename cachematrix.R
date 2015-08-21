## This R script finds the inverse of a given matrix and stores in a cache
## Next time when we try to find the inverse of the same matrix it will load from the cache instead of recomputing

## The function makeCacheMatrix contains the getters and setters for the matrix and it's inverse.It also stores them in a list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## This function cacheSolve returns the inverse of a matrix from cache if its computed once else it will caluclate the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}
