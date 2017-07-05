## A pair of functions which facilitate matrix
## inversion caching



## Below you can find a function that creates a special matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) { # initializing an empty default matrix
        inv <- NULL   # and an object in the environment to be used later
        set <- function(y) { #defining the method of assignment in parent environment
            x <<- y
            inv <<- NULL
        } 
        
        get <- function() x #through lexical scoping R retrieves currently undefined 
                            #element stored in parent environment
        setinv <- function(solve) inv <<- solve  #setting and getting the inverse
        getinv <- function() inv
        list(set = set, get = get,  #combining the functions within a list to be 
             setinv = setinv,       #returned in the parent environment
             getinv = getinv)
    
}



## The below function (cacheSolve) takes the special matrix from the function above 
## (makeCacheMatrix) and  calculates its inverse. Has the matrix not changed, 
## in the event that the inverse has been calculated and cached, the result is  
## retrieved from the cache instead of being calculated again.

cacheSolve <- function(x, ...) {    #defining a function with arguments x and additional ones 
    inv <- x$getinv()               #attempting to retrieve the inverse by:
    if(!is.null(inv)) {             #checking whether or not it has already been calculated and stored
        message("getting cached data")  #in the parent environment and retrieving it from the cache if so.
        return(inv)
    }
    data <- x$get()                 #if not, calling previously defined setinv function 
    inv <- solve(data, ...)         #on input argument and returning the inverse of the matrix.   
    x$setinv(inv)
    return(inv)
}

