## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## These functions will create a special matrix object that can cache
## its inverse. Then it will compute the inverse of the special
## matrix returned. If the inverse has already been calculated 
## then the cachsolve should retrieve the inverse from the cache
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse  <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
        ## This function will compute the inverse of the special matrix
## returned by makeCacheMatrix above. If inverse has already been
## calculated, then the cachesolve will retrieve the inverse from 
## the cache.
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
}
