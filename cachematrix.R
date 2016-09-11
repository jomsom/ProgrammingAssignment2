## Following two functions when using together calculates inverse of an invertible matrix
## and caches the inverse matrix so that user can get the cached value instead of 
## calculating the inverse of same matrix again when user needs that value again.


## makeCacheMatrix(x) takes invertible matrix, 'x', as an argument, and returns a 
## list of functions to set the matrix, get the matrix, set the inverse of the  
## matrix, 'x', and getting the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(newX) {
        x <<- newX
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, 
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve(x) takes output of 'x = makeCacheMatrix(m)' and chech if inverse of 
## the matrix 'm' is calculated. If it is not then calculates the inverse otherwise
## it returns cached value of the inverse of matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the argument matrix in makeCacheMatrix(m)
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
