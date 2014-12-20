### Introduction

This second programming assignment will require you to write an R
function that is able to cache potentially time-consuming computations.
For example, taking the mean of a numeric vector is typically a fast
operation. However, for a very long vector, it may take too long to
compute the mean, especially if it has to be computed repeatedly (e.g.
in a loop). If the contents of a vector are not changing, it may make
sense to cache the value of the mean so that when we need it again, it
can be looked up in the cache rather than recomputed. In this
Programming Assignment you will take advantage of the scoping rules of
the R language and how they can be manipulated to preserve state inside
of an R object.

### Example: Caching the Mean of a Vector

In this example we introduce the `<<-` operator which can be used to
assign a value to an object in an environment that is different from the
current environment. Below are two functions that are used to create a
special object that stores a numeric vector and caches its mean.

The first function, `makeVector` creates a special "vector", which is
really a list containing a function to

1.  set the value of the vector
2.  get the value of the vector
3.  set the value of the mean
4.  get the value of the mean

<!-- -->

makeVector <- function(x = numeric()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setmean <- function(mean) m <<- mean
getmean <- function() m
list(set = set, get = get,
setmean = setmean,
getmean = getmean)
}

The following function calculates the mean of the special "vector"
created with the above function. However, it first checks to see if the
mean has already been calculated. If so, it `get`s the mean from the
cache and skips the computation. Otherwise, it calculates the mean of
the data and sets the value of the mean in the cache via the `setmean`
function.

cachemean <- function(x, ...) {
m <- x$getmean()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- mean(data, ...)
x$setmean(m)
m
}

### Assignment: Caching the Inverse of a Matrix



`makeCacheMatrix` - In addition to setter/getters , it uses a matequal()
function to check if a new matrix is being set, if so it sets the matrix inverse
to NULL. This forces a NULL for matrix inverse , which results in a new inverse
computation.

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


cacheSolve - Uses isinvertible to check if the new matrix can be inverted


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


Please check `output.txt` for sample run output