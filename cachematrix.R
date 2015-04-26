## Matrix inversion can be a costly operation. The following code provides a way to cache the inverse of
## a matrix rather than have to compute it repeatedly. The first function, 'makeCacheMatrix', creates a
## special kind of matrix that can cache the matrix inverse. The second utility, 'cacheSolve', computes
## the inverse of the special matrix. If it has already been cached, then this calculation step is skipped.
## Example:
## A <- matrix(trunc(rnorm(512*512)*100), 512,512)
## CachedInv <- makeCacheMatrix(A)
## Ainv <- cacheSolve(CachedInv)

## This first function, makeCacheMatrix, creates a "special" matrix, which is really a list
## containing functions to: 1) Set the value of the matrix. 2) Get the value of the matrix. 3) Set
## the value of the inverse. 4) Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function computes the inverse of the special "matrix" created with the above function.
## First, it checks to see if the inverse has already been computed. If so, it gets the inverse from the
## cache and skips the above computation. Otherwise, it calculates the inverse and sets its value in the
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached inverse")
    return(invx)
  }
  X <- x$get()
  invx <- solve(X, ...)
  x$setinverse(invx)
  invx
}
