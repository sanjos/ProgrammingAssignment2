## This contains two functions. makeCacheMatrix creates a matrix object 
## and contains functions to get and set the mean. cacheSolve returns a cached
## inverse of a matrix if it exists, or else it calculates and returns a matrix inverse.

## makeCacheMatrix function creates a matrix object, has get and set functions for both
## the matrix and for its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
}


## cacheSolve returns the inverse of a matrix if it is already cached. If it is not cached
## then it calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
           data <- x$get()
           m <- solve(data, ...)
           x$setinverse(m)
           m
}
