## These functions create a special matrix that can
## store and retrieve its inverse

## Create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        x_i <- NULL
        set <- function(y) {
            x <<- y
            x_i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) x_i <<- inverse
        get_inverse <- function() x_i

        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse
             )
}


## Calculate or retrieve a special matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_i <- x$get_inverse()
        if(!is.null(x_i)) {
            message("getting cached data")
            return(x_i)
        }

        data <- x$get()
        x_i <- solve(data, ...)
        x$set_inverse(x_i)

        x_i
}
