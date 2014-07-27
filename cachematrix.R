## The following two funtions are used to create a special object that stores a
## matrix and cache's the inverse of the matrix. 

## makes a special matrix which is really a list containing a function to set 
## the value of the matrix, get the value of matrix, set the value of the 
## inverse of the matrix, and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        ##create the list that holds all the functions that constitute makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix  created with
## makeCacheMatrix. It first checks if the inverse matrix has already been
## calculated. If so it skips the computation and gets the value from the cache.
## Otherwise, it calculates the inverse matrix and sets the value in the cache 
## via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ##check for cached data and return cache if it exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##if the cache was empty need to get the matrix and solve for inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
