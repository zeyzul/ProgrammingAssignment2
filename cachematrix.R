## Caching the inverse of a matrix

## This function creates a special "matrix" that can cache its inverse.
## Just like in makeVector, it contains a list of functions,
## for getting and setting the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This following function, like the cachemean in the example, calculates
## the inverse of the special "matrix" created with the above function. 
## First checks if the inverse has been already calculated, if so gets its from cache
## if not then calculates the inverse and sets the value of the inverse in the cache
## with the help from the function above

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix) 
        x$setmean(i)
        i
}

