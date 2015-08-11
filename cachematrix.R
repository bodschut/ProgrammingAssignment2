## Functions for creating a special kind of matrix object which is able to store its inverse into the
## cache. In this way, the inverse must only be calculated once and when the inverse is needed multiple
## times (e.g. in a loop), it can be retrieved from the cache without the need to calculate it again.

## Function for creating the special matrix object. In essence, the object is a list with four functions that
## allow to get and set the matrix and to get and set the value for its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inv <<- inv
    getInverse <- function() inv
    list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## Function that calculates the inverse of a special matrix object created with the function above. It checks
## whether the inverse has already been calculated (i.e. if the value retrieved through getInverse() is not NULL).
## If so, the inverse is retrieved from the cache and computation is skipped. Otherwise, the inverse is computed and
## is stored in the cache through the setInvers function of the matrix object.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        print("Getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
