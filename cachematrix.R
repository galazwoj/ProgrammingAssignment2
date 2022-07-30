## this functions creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL
        set <- function(y) {
                x <<- y
                cached_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cached_inverse <<- inverse
        getinverse <- function() cached_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
        } else {
	        data <- x$get()
	        inverse <- solve(data, ...)
	        x$setinverse(inverse)
	}
        inverse
}

#
#> m = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#> x <-m$get() 
#> y <- cacheSolve(m)
#> x
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> y
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# check this with https://matrix.reshish.com/inverse.php
#
#> x%*%y
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
#> y <- cacheSolve(m)
#getting cached data
#> x%*%y
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
#
