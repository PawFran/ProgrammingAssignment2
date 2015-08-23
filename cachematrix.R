## purpose of this function is to use lexical scoping
## to cache the inverse of a given matrix to avoid
## time-consuming computations

## first function makes 'matrix' which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## takes matrix returned by the first function and
## retrieves its inverse from the cache 
## (or computes, if there is none)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)     ## cache inverse if there is any
    }
    data <- x$get()
    inv <- solve(data, ...) ## compute inverse if there isn't any
    x$setinverse(inv)       ## and set for further usage
    inv     
}
