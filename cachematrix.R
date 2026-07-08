## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions:
##   set        - set the value of the matrix
##   get        - get the value of the matrix
##   setinverse - set the value of the cached inverse
##   getinverse - get the value of the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), it retrieves the inverse from the
## cache instead of recomputing it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Quick test:
## m <- makeCacheMatrix(matrix(c(2,0,0,2), 2, 2))
## cacheSolve(m)   # computes and caches
## cacheSolve(m)   # prints "getting cached data", returns cached inverse
