## The following functions: (a) create a special "matrix" object ("makeCacheMatrix") that can cache its inverse
## and (b) compute the inverse of the special "matrix" returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and some associated sub-functions/methods
        ## define the cache inv
    inv <- NULL
    set <- function(y) {
        x <<- y ## assign the input matrix y to the variable x in the
                ## parent environment
        inv <<- NULL ## re-initialize inv in the parent environment to null
    }
    get <- function() x ## return the matrix x
    setInverse <- function(inverse) inv <<- inverse ## set the cache inv equal
        ## to the inverse of the matrix x
    getInverse <- function() inv ## return the cached inverse of x
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

        
## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}      
        
