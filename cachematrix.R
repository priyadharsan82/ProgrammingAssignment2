## The first function, makeCacheMatrix() creates a matrix and stores its inverse. The second function cacheSolve()
## retrieves the inverse of the matrix from the cache, or computes and caches the inverse and returns it

## makeCacheMatrix() builds and returns a list of functions that are used to create a matrix, store its inverse 
## and then later access them. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
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


## cacheSolve() takes an object returned by makeCacheMatrix() as argument and retrieves the cached inverse, if any,
## or computes and caches the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv   ## Return a matrix that is the inverse of 'x'
}
