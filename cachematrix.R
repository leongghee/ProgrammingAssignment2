## A pair of functions that will catch the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse  property
    i <- NULL
    
    ## set the matrix
    set <- function(i) {
        x <<- i
        i <<- NULL
    }
    ## get the matrix
    get <- function() m
    
    ## set the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
    ##get the inverse of the matrix
    getInverse <- function() i
    
    
    ##return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## return a matrix that is the inverse of "x"
    m <- x$getInverse()
    
    ##return the inverse if it is already set
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##get the matrix from our object
    data <- x$get()
    
    ##calculate the inverse of matrix
    m <- solve(data, ...)
    
    ## set the inverse to the object
    x$setInverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
    
}
