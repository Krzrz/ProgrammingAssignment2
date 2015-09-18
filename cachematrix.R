## Function makeCacheMatrix takes as an argument a square matrix
## and creates for it two variables to cache this matrix and its inverse. 
## As an output makeCacheMatrix gives a vector of four functions: 
## setting and getting value of the first variable (matrix argument), 
## and setting and getting the second variable (the inverse).

makeCacheMatrix <- function(x = matrix()) {
			## take square matrix as an argument
        m <- NULL

			## build auxiliary functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

			## return vector of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve takes as an argument value of makeCacheMatrix
## on a square matrix (so it gets access to "matrix" and "inverse" variables).
## It simply checks if the inverse variable is nonempty (was computed
## before), if it is, then returns it; if not, then it solves the matrix, sets
## the "inverse" variable and return its value.

cacheSolve <- function(x, ...) {
			## as an argument take value of
			## makeCacheMatrix function on a square matrix 
        m <- x$getinverse()

			## check if the inverse is already computed,
			## if it is, return it
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }

			## if it is not, compute and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

