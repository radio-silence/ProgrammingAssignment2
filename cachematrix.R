## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly

## makeCacheMatrix - prepares a matrix object
## input: 
##   a matrix (preferrably square invertible one)
## output:
##   $data - the input matrix
##   $inverse - inverse of the input matrix
##   $setinverse - should not have been exposed, just dont touch it
## example:
##   makeCacheMatrix(matrix(1:4,2))
##   makeCacheMatrix(matrix(rnorm(10000),100))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    data <- function() x
    setinverse <- function(inverse) inv <<- inverse
    inverse <- function() inv
    list(data = data,
         setinverse = setinverse,
         inverse = inverse)
}


## cacheSolve - invertes the prepared matrix
## input:
##   prepared matrix object (see makeCacheMatrix)
## output:
##   inverse of the matrix (if it is square and invertible)

cacheSolve <- function(x, ...) {
    inv <- x$inverse()
    if(is.null(inv)) {
        inv <- solve(x$data())
        x$setinverse(inv)
    } else {
        message("getting cached data")
    }
    inv
}
