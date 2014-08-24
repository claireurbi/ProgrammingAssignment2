## Below are the two functions that cache the inverse of a matrix for Programming Assignment 2
## We are to assume that the matrix supplied is always invertible

## The first function creates a special "matrix" object that can cache its inverse, see it below

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <-function(y){
    x <<-y
    m <<-NULL
  }
  get <-function()x
  setinverse <-function(solve) m <<-solve
  getinverse <-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The second function computes the inverse of the special "matrix" object returned by the function above
## If the inverse has already been calculated (and the matrix has not changed), then the function below
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  m
}
