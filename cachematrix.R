## These functions are designed to save time on potentially cumbersome calculations. In this case, they calculate
## and stores (caches) inverted matrices

## The first function, `makeCacheMatrix` creates a special "matrix", creates a special "matrix" object that can cache 
## its inverse-- which really a list containing a function to do the following: 

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverted (solved) matrix
## 4.  get the value of the inverted (solved) matrix

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setsolve <- function(invMtx) cachedInv <<- invMtx
    getsolve <- function() cachedInv
    list(set = set, get = get,
         setsolve = setsolve, 
         getsolve = getsolve)
}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
