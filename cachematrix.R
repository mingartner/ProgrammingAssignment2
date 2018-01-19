## The two functions here are designed to caching the inverse of a Matrix, so program
## does not have to do the calculation everytime the inversion was called.

## The first function is to create a list of four functions to: set and get the value of matrix;
## set and get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The 2nd function here is to first evaluate if the inverse of a matrix is in the cache, then: if yes,
## it retrieve the inverse from cache; if not, it calculate the inverse and stored in in cache.

cacheSolve <- function(x, ...) {
        i <- x$get_inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inverse(i)
        i    ## Return a matrix that is the inverse of 'x'
        
}
