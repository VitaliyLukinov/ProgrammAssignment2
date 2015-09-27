## This file contains collection of functions to inverse a square matrix:
#   
# 1. makeCacheMatrix() is a list of functions to operate with cached matrix
#
# 2. cacheSolve() inverses a square matrix with help of functions in makeCacheMatrix()
#
# Example
#
#> source("cachematrix.R")
#> x <- matrix(c(4,0,0,2), ncol=2, nrow=2)
#> y<-makeCacheMatrix(x)
#> z<-cacheSolve(y)
#> z 


## makeCacheMatrix() is a list of 4 functions for cacheSolve()
#
#  Input function:
#                  set(x) is put matrix x
#                  setinverse(inv) is put inverse matrix of x
#
#  Output data:
#                  get(x) is return matrix x
#                  getinverse(inv) is return inverse matrix of x            
makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
        if(all.equal(x, y) ){
            message("matrix are equal, using cached inverse matrix")  
        } else
        {
            x <<- y
            x.inverse <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inv) x.inverse <<- inv
    getinverse <- function() x.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve() inverse a square matrix x
#
#  Input data:
#              x is a list containing square matrix created by makeCacheMatrix(). 
#              WARNING: DON`T USE ATIOMIC MATRIX x !!!
#  
#  Output data:
#              inverse of square matrix contained in list x. 
#               

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    x.inverse <- x$getinverse()
    if(!is.null(x.inverse)) {
        message("getting cached inverse matrix")
        return(x.inverse)
    }
    data <- x$get()
    if (is.null(data)) {
        message("can't inverse null matrix")
        return(NULL)
    }
    x.inverse <- solve(data)
    x$setinverse(x.inverse)
    return(x.inverse)
}