## The first function takes a matrix as input, calculates inverse and produces 
## a list containing both the original matrix and the inverse matrix. 
## The second function takes the produced list and will either produced the
## cached inverse if already calcuated and if it is not present will
## calculate it.

## This function takes in a matrix, calculates its inverse, and outputs both 
## the original and inverse matrix as functions in a list. 

makeCacheMatrix <- function(x = matrix()) {
    CachedInverse <- solve(x)
    inverse <- function () CachedInverse
    matrix <- function () x
    list(matrix = matrix, inverse = inverse)
}


## This function takes the a list and if present, will retrieve the inverse
## matrix cached in the list. If it is not present it will calculate the
## inverse matrix from the original matrix present in the list.

cacheSolve <- function(x, ...) {
    inv <- x$inverse()
    if(!is.null(inv)){
        message("Retrieving cached inverse")
        return(inv)
    }
    else{inv <- solve(x$matrix())}
    inv
}
