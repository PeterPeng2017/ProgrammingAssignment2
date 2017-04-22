## The 2 functions below show how to cache a computation result in R laguange


## This function wraps a matrix to an object that can cache the its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL    
    }
    
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function first checks if the reverse of the matrix has already been cached. If cached,
## it will get the result from cache, otherwise, it computes the inverse of the matrix by the 
##function "solve" and set the result computed into the matrix's cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    
    inverse
}
