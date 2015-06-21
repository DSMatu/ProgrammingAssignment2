## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## Constructor    
    iM <- NULL
    set <- function(y){
        x <<- y
        iM <<- NULL
    }
    get <- function() x
    
    ## Method to compute the inverse  
    setinverse <- function(solve) iM <<- solve
    
    ## Method to return the inverse
    getinverse <- function() iM
    
    ## Build the special matrix object as a list  
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get the inverse field from the object
    iM <- x$getinverse()
    
    ## if the inverse has been calculated, then return the cached information
    if (!is.null(iM)){
        message("Getting cache data")
        return(iM)
    }
    
    ## otherwise calculate the inverse and cache it in the object
    data <- x$get()
    iM <- solve(data, ...)
    x$setinverse(iM)
    iM
}
