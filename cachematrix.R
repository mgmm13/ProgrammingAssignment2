## The following functions below provide the capability to compute 
## a matrix inverse, utilizing a cache to return data if matrix is 
## unchanged. 

## This function is based completely on the makeVector example from 
## https://github.com/rdpeng/ProgrammingAssignment2 . This will return
## an object that keeps track of its matrix data and its matrixt inverse 
## accessible via a list of get, set, getinverse and setinverse functions.
##
## To run, you pass a matrix object as parameter, as below example:
## > m1 <- matrix(1:4, 2, 2)
## > makeCacheMatrix(m1)

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(mInverse) i <<- mInverse
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function is based completely on the cachemean example from
## https://github.com/rdpeng/ProgrammingAssignment2 . This will return
## the inverse of the matrix from the cache if the matrix is unchanged. Otherwise
## it will recompute new inverse using the solve function. Function follows 
## the provided assumption that matrix input is invertible. 
##
## To run, you pass a makeCacheMatrix value as parameter, as below example:
## > m1 <- matrix(1:4, 2, 2)
## > f1 <- makeCacheMatrix(m1)
## > cacheSolve(f1)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

   i <- x$getinverse()
   if(!is.null(i)) {
      message("Getting cached data (inverse of matrix)...")
      return(i)
   }

   data <- x$get()
   i <- solve(data)
   x$setinverse(i)
   i
}
