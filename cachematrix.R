## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize the stored inverse value to NULL
        inverse <- NULL
        # set value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get value of matrix
        get <- function() x
        # set inverse of matrix
        setinverse <- function(i) inverse <- i
        # get inverse of matrix
        getinverse <- function() inverse
        # return a list containing all functions defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get inverse
        inverse <- x$getinverse()
        # if inverse exists, return cached inverse
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        # if not, get matrix
        data <- x$get()
        # compute inverse of matrix
        inverse <- solve(data, ...)
        # cache inverse of matrix
        x$setinverse(inverse)
        # return inverse
        inverse
}
