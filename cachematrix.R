## These two functions provide an object that stores and retrieves a matrix
## value along with its inverse (makeCacheMatrix) as well as a function that
## tests if the matrix object has already had its inverse evaluated (cacheSolve)
##. If so, it returns that stored value. If not, it calculates the inverse and 
## stores it in the matrix object.

## makeCacheMatrix creates the matrix object. Its parameters are:
## x: the stored matrix value
## inv: the inverse of x
## 
## Its methods are:
## get: return the value of x
## set: set the value of x
## getinverse: return the value of the inverse stored in the object
## setinverse: set the value of the inverse to the method argument

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverted) inv <<- inverted
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes an instance of makeCacheMatrix and returns
## the value of the inverted matrix. If the matrix inverse has 
## not been calculated, it uses solve to find the value of the
## inverse and stores it in the matrix object. Otherwise, it 
## returns the value of the inverse stored in the object.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
