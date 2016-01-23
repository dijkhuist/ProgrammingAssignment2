## the inverse of a matrix, inspired by the vector example given in the assignment
## Below two functions are used to create a matrix and caches it and its inverse

makeCacheMatrix <- function(x) {
        ivs <- NULL
        set <- function(y) {
                x <<- y
                ivs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) ivs <<- inverse
        getInverse <- function() ivs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve creates the inverse of the matrix makeCacheMatrix above. 
##If the inverse already exists then the matrix is taken from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivs <- x$getInverse()
        if (!is.null(ivs)) {
                message("getting cached data")
                return(ivs)
        }
        mat <- x$get()
        ivs <- solve(mat, ...)
        x$setInverse(ivs)
        ivs
}
