## There are two functions below:
## 1.  makeCacheMatrix creates a special "matrix" object that
## can cache its inverse.
## 2.  cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## The first function, makeCacheMatrix, creates a special "matrix", 
## which is really a list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
