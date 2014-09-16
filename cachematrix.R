## Put comments here that give an overall description of what your
## functions do

## The following two functions are used for the calculation of the
## inverse of a matrix by using the function solve, and using the
## cache to speed the process without calculating the inverse over and
## over for repeated times.

## Write a short comment describing this function

## This function creates a list of functions to set the original matrix,
## get the original matrix, set the inverse matrix with the function
## solve, and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    setorig <-function(y) {
        x<<-y
        inv<<-NULL
    }
    getorig<-function() x
    setinv<-function(solve) inv<<-solve
    getinv<-function() inv
    list(setorig=setorig, getorig=getorig, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## This function looks if there is a matrix in the cache. If there is,
## it prints out a message and it returns that function. If not, it
## calculates the inverse and returns that inverse.

cacheSolve <- function(x, ...) {
    inv <-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    toinvert<-x$getorig()
    inv<-solve(toinvert,...)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}