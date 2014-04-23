## Put comments here that give an overall description of what your
## functions do

## function makeCachmatrix creates a special "matrix", which is really
## a list containing a functions to get, set, getinverse and setinverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    inverseMatrix <- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverseMatrix <<- inv
  getinverse <- function() inverseMatrix
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve calculates inveser matrix of the special object
## creaded with the above function However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse
##  from the cache and skips the computation. Otherwise, it calculates 
## the inveese matrix of the data and sets the value of the invese in 
## the cache via the `setinvese` function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
