## A pair of functions which facilitate matrix
## inversion caching



## Below you can find a function that creates a special matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    
}



## The below function (cacheSolve) takes the special matrix form the function above 
## (makeCacheMatrix) and  calculates its inverse. Has the matrix not changed, 
## in the event that the inverse has been calculated and cached, the result is  
## retrieved from the cache instead of being calculated again.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}

