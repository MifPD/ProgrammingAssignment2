
## Function "makeCacheMatrix" is a set of other functions to:
##  set the value of a matrix
##  get the value of a matrix
##  set the inverse of a matrix
##  get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_cache <- NULL
        set <- function(y) {
                x <<- y                    
                inv_cache <- NULL     
        }
        get <- function() x
        setinverse <- function(inv_calc) inv_cache <<- inv_calc
        getinverse <- function() inv_cache
        list( set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse )
}

## Function "cacheSolve" is used for getting cashed inverse of a matrix 
## or caching the inverse of matrix

## To use this function you need to do following steps (for using is.singular.matrix() function):
##     1. install.packages("matrixcalc")
##     2. library(matrixcalc)
## Or you can replace the first if condition with "abs(det(x$get())) > 1e-12" without quotes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if( !is.singular.matrix(x$get()) ){
                inv_calc <- x$getinverse()
                if(!is.null(inv_calc)) {
                        message('getting cached inverse matrix')
                        return(inv_calc)
                }
                inv_calc <- solve(x$get(), ...)
                x$setinverse(inv_calc)
                inv_calc
        }
        else{
                message('matrix is singular')
        }
}
