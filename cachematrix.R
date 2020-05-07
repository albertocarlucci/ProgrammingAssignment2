## Overall Description: Caching the Inverse of a Matrix
## The first function called makeCacheMatrix will create a special "matrix" object
## and then caches its inverse.
## The second function called cacheSolve retrieves the inverse from the cache if calculated already.
## If it was not calculeted yet or the matrix has changed, cachesolve computes the inverse of the matrix.

## Initializing of two objects. x = matrix() as a function argument and inv as NULL.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##definition of set and get functions
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## First checking is is empty. If not, this function retrieves data from cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##Computing the inverse with the function solve and then setting the result into object inv.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
