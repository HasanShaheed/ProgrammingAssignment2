## caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invs_x <- NULL
    set <- function(y) {
    x <<- y
    invs_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) invs_x <<-inverse
  getinverse <- function() invs_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs_x <- x$getinverse()
  if (!is.null(invs_x)) {
    message("getting the cached inverse matrix data")
    return(invs_x)
  } else{
    invs_x <- solve(x$get())
    x$setinverse(invs_x)
    invs_x
  }
}

