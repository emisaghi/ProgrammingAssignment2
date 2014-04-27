## These functions are written to calculate the matrix inversion and
## to get the inverse from the cache, if it has been calculated before.

## This function makes a matrix and returns a list of four functions to
## get and set the values inside the matrix and also to get and set the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function uses the above function to calculate the inverse of the matrix.
## If the matrix inverse is not null (i.e. it is calculated before), the function
## returns the value. If it is null (i.e. it's not calculated before), it calculates
## the inverse of the matrix using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m) ## Here the function returns the inverse if it is calculated before and stops.
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) ## Here we set the inverse of the matrix using the above function
  m               ## and then return the value.
}
