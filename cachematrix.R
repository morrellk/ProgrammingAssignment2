## The functions in this file support cache memory storage of a matrix 
## and its inverse.  This allows the inverse to be
## retrieved rather than recalculated on subsequent calls.
##
##  NOTE:  The matrix is assumed to invertible.  The R function solve()
##  will return an error if the matrix is not square or not invertible.
##

## makeCacheMatrix() defines a list of functions used to assign and retrieve 
## a matrix and its inverse from memory.  
## Usage example:
##   Initial assignent:  mat_cache <- makeCacheMatrix(mat) 
##                       # where mat is matrix of interest
##   Retrieving matrix:  mat_cache$get()
##   Storing inverse:    mat_cache$setinv(solve(mat))
##   Retrieving inverse: mat_cache$getinv()   
##

makeCacheMatrix <- function(mat = matrix()) {
     ## 'mat' is a matrix, ASSUMED INVERTIBLE for which
     ## the matrix inverse will be stored
     inv <- NULL
     
     set <- function(y) {
          # when set is called, the inverse is set to NULL
          mat <<- y
          inv <<- NULL
     }
     get <- function() mat
     setinv <-function(inverse) inv <<- inverse
     getinv <- function() inv
     
     ## Return value is a list of the functions defined here
     list(set = set, get = get, setinv = setinv, 
          getinv = getinv)
}



## cacheSolve() takes a function list created by makeCacheMatrix as its 
## argument and returns the matrix inverse for the matrix associated with
## that list.
##
## If the matrix inverse has been previously calculated and cached, 
## it is recalled from memory.  If the matrix inverse has not been
## saved previously, it is calculated and saved to cache by this function.

## Usage:
##   Original assignment: mat_cache <- makeCacheMatrix(mat)
##   Calling cacheSolve:  mat_inv <- cacheSolve(mat_cache)
##                        # will assign inverse of mat to mat_inv
##
## 

cacheSolve <- function(x, ...) {
     ## 'x' is the list of functions used to access to the matrix to be inverted
     ## cacheSolve returns a matrix that is the inverse of 
     ## the matrix used to create x.
     
     ## First call getinv
     inv <- x$getinv()
     ## if the return value is not NULL, inverse is available
     if (!is.null(inv)){
          message("retrieving matrix inverse from cache")
          return(inv)
     }
     
     ##  If the inverse has not yet been stored, retrieve the matrix and 
     ##  use solve() to calculate the inverse, then store the result.
     mat <- x$get()
     inv <- solve(mat,...)
     x$setinv(inv)
     inv     
}
