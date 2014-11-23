## The 2 below functions create a special matrix object which 
## stores a matrix along with calculating and storing
## the inverse of that matrix. This is so the calculation
## only needs to be performed one time for a particular matrix, 
## and any subsequent calls to solve for the inverse can simply 
## be retrieved from the cache

## The first function is "makeCacheMatrix" which creates the matrix
## that also caches its inverse. It contains a list with 4 functions to 
## do the following: 
## 1) set the input matrix 
## 2) get the input matrix
## 3) set the inverse matrix
## 4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    mx_inverse <- NULL
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mx_inverse <<- inverse
    getinverse <- function() mx_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The "cacheSolve" function calculates the inverse matrix for 
## the special matrix (x) created in the "makeCacheMatrix" function.
## Before calculating, it checks if the inverse has already been
## calculated, and if so, returns the inverse from the cache instead
## of re-calculating it. If not already cached, it calculates the 
## inverse using the solve() function, and caches it for later retrieval. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mx_inverse <- x$getinverse()
    if(!is.null(mx_inverse)) {
        message("getting cached data")
        return(mx_inverse)
    }
    data <- x$get()
    mx_inverse <- solve(data, ...)
    x$setinverse(mx_inverse)
    mx_inverse
}
