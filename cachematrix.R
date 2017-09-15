## Functions to calculate de inverse of a square matrix 
## and store it on cache to avoid further calculations


## Creates a special matrix thats calculates it's inverse and stores
## it in cache 

makeCacheMatrix <- function(x = matrix()) {
    # varible initialization
    # inverse of the matrix
    inverse <- NULL
    
    # set the data for the matrix
    # and initialize the value of the inverse
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the matrix data
    getMatrix <- function() x
    
    # set the calculated inverse for the matrix
    setInverse <- function(inverse) inverse <<- inverse
    
    # get the calculated inverse
    getInverse <- function() inverse
    
    # returns  a list with all the functions for the special
    # makeCacheMatrix object
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Uses a makeCacheMatrix object to calculate the inverse of a square matrix
## if has already been calculated then retrieves it from cache

cacheSolve <- function(x, ...) {
    
    # gets the inverse of the matrix
    inverse <- x$getInverse()
    
    # if the inverse is not null then gets the value from cache and returns
    if(!is.null(inverse)) {
        message("Getting from cache")
        return(inverse)
    }
    
    # if the inverse is not calculated then invokes the solve function
    matrix <- x$getMatrix()
    message("Processing...")
    inverse <- solve(matrix, ...)
    
    # sets the value of the calculated inverse
    x$setInverse(inverse)
    
    # returns the inverse
    inverse
}
