## The makeCacheMatrix function produces a list of functions
## serve as inputs for the cacheSolve function. The cacheSolve
## function will compute the inverse of the matrix given as input 
## to the makeCacheMatrix function. 

## The makeCacheMatrix function will create the following 
## functions: set(), get(), setinverse(), and getinverse()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## The cacheSolve function will take the list of functions from
## makeCacheMatrix and check to see if the inversion of the matrix
## is currently in cache. If not, it will perform the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
