## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invInCache <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(invSolved) invInCache <<- invSolved
        getInverse <- function() invInCache
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invFromCache <- x$getInverse()
        if(!is.null(invFromCache)) {
                message("getting cached inverse of matrix (x).") 
                return(invFromCache)
        }
        data <- x$get()
        invToCache <- solve(data)
        x$setInverse(invToCache)
        invToCache
}
