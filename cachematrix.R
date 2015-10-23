## R Programming Assignment 2 cache matrix inversion 


## Function to cache a matrix, and cache the inverse once calculated

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

	## set function to cache the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	## get function to return the matrix
        get <- function() x

	## setinv function, cache the inverse of the matrix
        setinv <- function(minv) m <<- minv

	## getinv function to return the cached inverse matrix
        getinv <- function() m

	## return a list of functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Function to solve for the inverse of a matrix
## once calculated, cached results are returned
## Input is the output of makeCacheMatrix 

cacheSolve <- function(x, ...) {

	## get the cached inverse, and return it if it exists
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## no cached inverse, so get the original matrix
        data <- x$get()

	## use the solve funtion to calculate the inverse of the matrix
        m <- solve(data, ...)

	## cache the inverse matrix
        x$setinv(m)

	## return the inverse matrix that was just calculated
        m
}

