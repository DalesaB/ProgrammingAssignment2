## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function creates a special "matrix" object that can cache its 
## inverse. 

## This function gets a matrix as an input, sets the value of the
## matrix, gets the value of the matrix, and lastly gets the
## inverse matrix. 

## Use of the <<- operator assigns a value to an object in an environment
## that is different from the current environment 

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
                invMatrix <- NULL           ## initialize invMatrix as NULL to hold value of inverse
                
                setMatrix <- function(y) {  ## define the set function to assign new value of
                             x <<- y        ## matrix in parent environment
                             invMatrix <<- NULL  ## if there is a new matrix, reset inverse to NULL
                }

                getMatrix <- function() x    ## define the get function to return value of matrix argument
                setInverse <- function(inverse) invMatrix <<- inverse    ## assign value of inverse in parent environment
                getInverse <- function() invMatrix                       ## get value of inverse
                list(setMatrix = setMatrix, getMatrix = getMatrix, 
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## Write a short comment describing this function

## This function calculates the inverse of the special "matrix" returned
## by the makeCacheMatrix function above. If the inverse has already been
## calculated, the function will get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           invMatrix <- x$getInverse()
           
           if(!is.null(invMatrix)) {
                       message("getting cached data")
                       return(invMatrix)
           }
           
           dataMatrix <- x$getMatrix()
           invMatrix <- solve(dataMatrix, ...)
           x$setInverse(invMatrix)
           invMatrix
}

## *****TESTING (w 2x2 matrix)*******

testmatrix <- matrix(1:4, 2, 2)
testmatrix

cachematrix <- makeCacheMatrix(testmatrix)
cachematrix$getMatrix()
cachematrix$getInverse()

cacheSolve(cachematrix) ## should return the inverse of 2x2 matrix