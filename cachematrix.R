makeCacheMatrix <- function(x = matrix()) {
        ## x: square invertible matrix
        ## return a list containing functions to
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix()
        ## return the inverse of the original matrix
        
        m <- x$getmatrix()
        
        # if the inverse has already been calculated
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        #otherwise, calculate the inverse
        matrix <- x$get()
        m <- solve(matrix, ...)
        
        #sets the value of the inverse in the cache
        x$setmatrix(m)
        m
}
