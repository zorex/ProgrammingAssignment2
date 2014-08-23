## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function allows one to set the value of a matrix, get that value, set a matrix as the reverse of this matrix and get that reverse matrix

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setreverse <- function(reversed) r <<- reversed
    getreverse <- function() r
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}


## Write a short comment describing this function
## This function first tries to get the reverse of the matrix via the getreverse-function from makeCacheMatrix
## if that works, it returns the reverse matrix, adding that it's being retrieved from cache
## if it doesn't work (because the reverse matrix wasn't cached), it calculates the reverse using the solve-function and returns it
## and makes sure the result gets cached via the setreverse-function from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getreverse()
        if(!is.null(r)){
            message("getting cached data")
            return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setreverse(r)
        r
}
