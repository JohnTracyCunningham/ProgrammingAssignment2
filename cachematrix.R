## Function cachematrix.R by John Tracy Cunningham, 21 May 2014

## This function returns the inverse of a square matrix.  The input is
## assumed to be a square matrix.  Use makeCacheMatrix first to create a
## vector cache that will accept a matrix; then use cacheSolve to return the
## inverse.  If the inverse of a matrix X has already been calculated, it has
## been cached, and the cached inverse is not recalculated, but returned from the
## cache.  If the inverse has not already been calculated, it is now, one time
## only, and cached.  As calculating an inverse is resource-consuming, using a
## cache saves time.

## Create a vector cache with, for example, 
## a <- makeCacheMatrix(x = matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
        matinv <- list()
        length(matinv) <- nrow(x) * ncol(x)
        dim(matinv) = c(nrow(x), ncol(x))
        set <- function(y = matrix()) {
                x <<- matrix()
                x <<- y
                matinv <<- list()
                length(matinv) <<- nrow(x) * ncol(x)
                dim(matinv) <<- c(nrow(x), ncol(x))
        }
        get <- function() x
        matrixinverse <- matrix()
        setinv <- function(matrixinverse) matinv <<- matrixinverse
        getinv <- function() matinv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the inverse of the matrix with, for example, cacheSolve(a)

cacheSolve <- function(x = matrix(), ...) {
        matinv <- x$getinv()
        if(!is.null(matinv[[1, 1]])) {
                message("Getting cached data...")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinv(matinv)
        matinv
}
