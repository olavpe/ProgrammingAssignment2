## The first function takes a matrix as input, calculates inverse and produces 
## a list containing functions that will; set, get, set the inverse, get the
## inverse matrix. The second function takes the produced list and will
## produce the cached inverse matrix.


## This function takes in a matrix and outputs a list of functions to be 
## used to solve the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function takes the a list. If the matrix has already been calculated
## it will just retrieve the cached data, but if not it will execute the
## functions that calculate the inverse matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Retrieving Cached Data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}