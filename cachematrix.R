## This program is designed to return the inverse of a
## matrix. It will use caching to improve the performance
## when possible

## this function takes a matrix as an input and returns a
## list of functions that relate to the input matrix in
## one of 4 ways: redefinition, fetch, inverse definition,
## and inverse fetch.
makeCacheMatrix <- function(inputMatrix = matrix()) {
    inverse <- NULL
    setMatrix <- function(newInputMatrix) {
        inputMatrix <<- newInputMatrix
        inverse <<- NULL
    }
    getMatrix <- function () {
        inputMatrix
    }
    setinverse <- function(inverse) {
        inverse <<- inverse
    }
    getinverse <- function () {
        inverse
    }
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this function, given a matrix will call the matrix cache
## to find out if the inverse of the matrix has already been
## calculated if it has, it will fetch that value and return it.
## otherwise, it will calculate the inverse, store it into the
## cache, and return the value
cacheSolve <- function (inputMatrix, ...) {
    inverse <- inputMatrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- inputMatrix$getMatrix()
    inverse <- solve(data, ...)
    inputMatrix$setinverse(inverse)
    inverse
}
