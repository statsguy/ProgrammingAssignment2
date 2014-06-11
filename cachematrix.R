## "makeCacheMatrix" creates a list of functions to get and set the
## the matrix and its inverse
## "cacheSolve" returns the inverse of the matrix created by the 
## "makeCacheMatrix" if computed and cached before, otherwise computes and 
## caches it so that it is not computed again from scratch when needed 


## This function creates a special vector which is a list 
## containing a function to set the value of the matrix,
## get the value of the matrix, set the inverse of a matrix,
## and get the inverse of a matrix
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


## Calculates the inverse of a "matrix" created with above function named 
## "makeCacheMatrix". It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
