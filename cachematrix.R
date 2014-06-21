## 
## The makeCacheMatrix and cacheSolve functions in this R script
## together provide a way for creating a matrix, lazily computing
## its inverse, and caching the inverse on the matrix so it's not
## recomputed after the first time it is computed. In other words,
## the inverse is cached in order to avoid a potentially expensive
## re-calculation.
##
## Note that makeCacheMatrix creates a matrix capable of holding on
## to the cached inverse value. cacheSolve computes the inverse and
## sets it into the matrix.
##
## A sample usage pattern would be as follows:
##
## m <- matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=3)
## cm <- makeCacheMatrix(m)
## invM <- cacheSolve(cm)
##
## This creates a 3x3 matrix, generates our special "extension"
## capable of caching the computed inverse, then computes the inverse.
## A subsequent call into cacheSolve returns the cached inverse value:
##
## invM2 <- cacheSolve(cm)
##
## The "extended" matrix implementation also allows you to set a new
## matrix in. That clears the cached inverse and it'll be re-computed
## the next time the cacheSolve is invoked.
##

## *** Assumptions ***
## The implementation assumes that the input matrix is always invertible.
## This implies square, invertible matrix, only.

##
## *** Example ***
##
## m <- matrix(c(4, 7, 2, 6), nrow=2, ncol=2)
## > m
## [,1] [,2]
## [1,]    4    2
## [2,]    7    6
##
## cm <- makeCacheMatrix(m)
##
## > inv <- cacheSolve(cm)
## Getting a freshly computed inverse value.
## > inv
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
##
## > inv <- cacheSolve(cm)
## Getting cached inverse.
## > inv
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4

##
## Author's perspective:
##
## I believe we'd benefit from a more streamlined implementation where
## we'd do away with cacheSolve and the set method within makeCacheMatrix
## and just make the getinverse method compute the inverse then cache it,
## or just return the cached value if already computed.
##
## The reasons for that would be to:
## a) have less code
## b) avoid having to invoke cacheSolve every time that matrix value is reset
## c) encapsulate/protect the calculation of the inverse into the makeCacheMatrix
## logic, in order to avoid a possible frivolous setting of an incorrect
## inverse value via the setinverse call.
##
## However, the assignment calls for the specific implementation as below.
##

## Creates a matrix capable of holding on to a cached matrix inverse value.
## Input: a generic matrix
## Output: an "extended" matrix capable of holding on to a cached inverse
## See also: cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    ## Contains the cached value of the matrix inverse
    inv <- NULL
    
    ## Allows you to set the matrix in.
    ## Note: doing so clears any cached inverse. An invokation of cacheSolve
    ## is required to re-compute the inverse and cache it.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Returns the matrix.
    get <- function() x
    
    ## Sets in the computed matrix inverse.
    setinverse <- function(inverse) inv <<- inverse
    
    ## Returns the matrix inverse.
    getinverse <- function() inv
    
    ## Makes the methods available on the "extended" matrix implementation.
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Computes a matrix inverse and sets it into the matrix.
##
## *** Assumes that the matrix supplied is always invertible. ***
## This implies square, invertible matrix. No sanity checking is
## performed on the input, e.g. to check the matrix dimensions.
##
## Input: an "extended" matrix capable of caching the inverse value.
## Ouput: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    message("Getting a freshly computed inverse value.")
    inv
}

