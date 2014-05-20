## There are 2 functions i.e.
## a. makeCacheMatrix() -- This function creates a special 
##    "matrix" object that can cache its inverse.
## b. cacheSolve() -- This function computes the inverse 
##    of the special "matrix" returned by makeCacheMatrix 
##    If the inverse has already been calculated 
##    (and the matrix has not changed), then cacheSolve 
##    should retrieve the inverse from the cache.

## This function defines a structure to store the  inverse of 
## a matrix in cache.
## This internally returns a list of functions. There are
## 2 pairs of funtions, one to get/set the matrix to be inversed
## the other to get/set the inverse of the matrix provided.
makeCacheMatrix <- function(x = matrix()) {
   
  inverse <- NULL
   
   setMatrix <- function(y) {
         x <<- y
         inverse <<- NULL
   }
   
   getMatrix <- function() x
   
   setInverse <- function(inv) inverse <<- inv
   
   getInverse <- function() inverse
   
   list(setMatrix=setMatrix, 
        getMatrix=getMatrix, 
        setInverse=setInverse, 
        getInverse=getInverse)
}


## Write a short comment describing this function
## Compute the inverse of a square matrix. If the inverse
## is already computed, then return it from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        data <- x$getMatrix()
        
        ## Check if the inverse is not null 
        ## and the matrix to be inversed is
        ## the same as the one already computed.
        if(!is.null(inverse)) {
          message("Returning from cache")
          return (inverse)
        }
        
        ## Inverse needs to be computed and stored
        ## in the cache.    
        
        message("Computing inverse")
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
