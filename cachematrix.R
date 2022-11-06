## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                          ##initialize the value of the matrix to NULL
        set <- function(y) {               ## declare another function set, matrix is created for the first time 
                x <<- y
                i <<- NULL
        }
        get <- function() x                ##get the value of the inverse
        setinverse <- function(inverse) i <<- inverse   ## calculate value of the inverse
        getinverse <- function() i                 
        list(set = set,                            ##passes the value of the function makeCacheMatrix
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {              ## if the inverse matrix exists, it found it
                message("getting cached data")
                return(i)
        }                               ## otherwise, first it calculates and then retrrieved
        data <- x$get()                
        i <- solve(data, ...)
        x$setinverse(i)
        i
}       
        
        ## Return a matrix that is the inverse of 'x'

a <- matrix(1:4, nrow = 2, ncol = 2)

matrixf <- makeCacheMatrix(a)
matrixinv <- cacheSolve(matrixf)
matrixinv



