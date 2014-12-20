## makeCacheMatrix implements setter/getter for raw matrix and 
## inverse matrix 
## cacheSolve returns either the cached inverse or a calculated inverse. 
## If calculated, it caches as per specification

## makeCacheMatrix - In addition to setter/getters , it uses a matequal()
## function to check if a new matrix is being set, if so it sets the matrix inverse
## to NULL. This forces a NULL for matrix inverse , which results in a new inverse
## computation

makeCacheMatrix <- function(x = matrix()) {
    matrixinverse <- NULL
    
    ## Setter for matrix 
    set <- function(y) {
        if (!matequal(x,y)){
            message("overriding  previously stored matrix with new argument matrix, \
                    new inverse will be computed")
            x <<- y
            matrixinverse <<- NULL 
        } else {            
        }
    }
    
    ## Getter for matrix 
    get <- function() x
    
    ## Setter for setting matrix inverse 
    setinverse <- function(inverse) matrixinverse <<- inverse
    
    ## Getter for getting matrix inverse
    getinverse <- function() matrixinverse
    
    ## Check if two matrices are equal 
    matequal <- function(x, y)
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - Uses isinvertible to check if the new matrix can be 
## inverted

cacheSolve <- function(x, ...) {
    ## Helper to check if the matrix is invertible
    ismatrixinvertible <- function(m) class(try(solve(m),silent=T))=="matrix"
    matrix <- x$get()
    
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix data")
        return(inverse)
    } 
    
    if (ismatrixinvertible(matrix)){
        message("computing matrix inverse")
        inverse <- solve(matrix)
    } else {
        message("matrix cannot be invertible")
        return
    }
    x$setinverse(inverse)
    inverse
}
