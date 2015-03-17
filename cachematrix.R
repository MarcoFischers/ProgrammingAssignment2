## function makeCacheMatrix will create a cached matrix object  
## that can hold also the inverse of the matrix
## function cacheSolve will compute the inverse and store it
## in the cache or retrieve the inverse if already cached

## create matrix cache object for a square matrix 'x'
## the object provides functions for:
## set            reuse the matrix cache
## get            retrieve the matrix
## setinverse     store the inverse matrix
## getinverse     retrieve the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
		## check that parameter 'x' is a square matrix
		if( !is.matrix(x) )
				stop("parameter is not a matrix")
		if( nrow(x) != ncol(x) )
				stop("parameter is not a square matrix")

		## variable for caching the inverse of the matrix 'x'
		inv <- NULL
		
		## store matrix 'x' in cache and initialize inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		## retrieve matrix from cache
        get <- function() x
		
		## store inverse in cache
        setinverse <- function(inverse) inv <<- inverse
		
		## retrieve inverse from cache
        getinverse <- function() inv
		
		## create list with cache access functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## compute the inverse for a cached matrix object 'x'
cacheSolve <- function(x, ...) {
		## retrieve inverse from cache of 'x'
        inverse <- x$getinverse()
		
		## inverse already in cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
		
		## retrieve matrix from cache
        data <- x$get()
		
		## compute the inverse of the matrix 'x'
        inverse <- solve(data, ...)
		
		## store inverse in cache
        x$setinverse(inverse)
		
		## return inverse of 'x'
        inverse
}