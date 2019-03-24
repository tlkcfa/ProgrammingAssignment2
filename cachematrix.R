## These functions create a special metrix object that can cache its inverse

## The first function, makeCacheMatrix creates a special “matrix”, 
## this is a list containing a function that will
## 1. set the value of the matrix, 2. get the value of the matrix 
## 3. set the value of the inverse 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special maxtrix returned above
## if the inverse was calculated then this will retreieve the inverse 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## X <- matrix(c(1,2,3,4),2,2)
## > X1 <- makeCacheMatrix(X)
## > cacheSolve(X1)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


