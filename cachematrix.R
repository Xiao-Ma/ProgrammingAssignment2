## The functions are used to calculate the inverse of a matrix
## without repeated calculations when the matrix is unchanged.

## This function  creates a special vector which stores the matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates and returns the inverse of a matrix if
## the inverse has not been calculated, else get and return the 
## inverse.

cacheSolve <- function(x, ...) {
       
    inv <- x$getinv()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    
    inv
    
}
