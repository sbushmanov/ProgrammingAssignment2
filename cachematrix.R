################################################################################
# 
# Below is R code for a pair of functions that
#
# (1) makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
# (2) cacheSolve: computes inverse of the matrix unless it already exists in cache
# In this case inverse is retrieved from the cache. 
#
# Inverse is computed via  "solve()" function, assuming inverse of a matrix exists.
#
# The pair is meant to make use of cache to reduce CPU load and increase workload
# of coder
#
################################################################################

# makeCacheMatrix function creates a list of 4 functions to:
#       - set a matrix: makeCacheMatrix$set()
#       - get a matrix: makeCacheMatrix$get()
#       - set inverse of a matrix: makeCacheMatrix$setinverse()
#       - get inverse of a matrix: makeCacheMatrix$getinverse()
# makeCacheMatrix(x) function returns a list of 4 functions
# See examples in the Examples Section

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve function checks if inverse exists and:
#       - if not -- calculates and stores it
#       - if yes -- retrives from cache while printing message
# cacheSolve(x) function returns a matrix that is inverse of x
# See examples in the Examples section

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# Examples: 

set.seed(1)                     # set seed to ensure reproducibility
mat <- matrix(rnorm(9), 3)      # generate random matrix
tr <- makeCacheMatrix(mat)      # make an object
tr$get()                        # see it
mat                             # sanity check
tr$getinverse()                 # no inverse so far
cacheSolve(tr)                  # first attempt, inverse is calculted
cacheSolve(tr)                  # second attempt, invers retrieved from cache
identical(cacheSolve(tr), solve(mat))          # another sanity check
