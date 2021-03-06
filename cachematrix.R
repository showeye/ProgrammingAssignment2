## Function that creates list with stored given matrix and inverted data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inversed) inv <<- inversed
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Function that checks if inversion already exists, if not, it counts it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <<- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
