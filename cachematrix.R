## CACHING THE INVERSE OF A MATRIX
## Author: Sergiy Redchyts
## Date: 14/10/2015
## ------------------------------------------------------------------------------
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Pls find below the code as a part of an Assignment 2 of "R Programming"
## Coursera course that cache the inverse of a matrix.


## ------------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. 
## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_matrix <<- inverse
        getinverse <- function() inv_matrix
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)   ## It is a list containing a function
}


## ------------------------------------------------------------------------------
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {      ## To check does a cache of inverse matrix exists
                message("getting cached data.")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data)       ## It is assumed that the matrix is always invertible
        x$setinverse(inv_matrix)
        inv_matrix
}