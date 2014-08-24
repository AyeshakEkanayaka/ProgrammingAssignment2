## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## These are some functions to cache the inverse of a matrix


## This gives a special matrix which cache its inverse
makeCacheMatrix <- function( M = matrix() ) {
  
  ## Initializing the inverse property
  i <- NULL
  
  ##Setting the matrix
  set <- function( matrix ) {
    M <<- matrix
    i <<- NULL
  }
  
  ## Getting the matrix
  get <- function() {
    ## Getting the inverse of the matrix
  getInverse <- function() {
    ## Returning the inverse property
    i
  }
  
  ## Returning a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This cachesolve retrieve the inverse from the cache 
##if the inverse is already calculated and the matrix has not changed
cacheSolve <- function(x, ...) { 
  ## Returning a matrix that is the inverse of 'x'
  M <- x$getInverse()
  
  ## Returning the inverse if its already set
  if( !is.null(M) ) {
    message("getting cached data")
    return(M)
  }
  
  ## Getting the matrix
  data <- x$get()
  
  ## Calculating the inverse of matrix
  M <- solve(data) %*% data
  
  ## Setting the inverse
  x$setInverse(M)
  
  ## Returning the matrix
  M
}
