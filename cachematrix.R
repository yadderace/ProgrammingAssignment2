## Put comments here that give an overall description of what your
## functions do

## Create a matrix and save the original and inverse matrix, return a list of functions.
## When the set function is executed, first evualuate the new matrix with the previous matrix to know if they are
## the same

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    ##Verify that the new matrix and the previous matrix have the same number of columns and rows
    if(!is.null(x) && is.matrix(y) && ncol(x) == ncol(y) && nrow(x) == nrow(y)){
      
      ##Verify the data in both matrix
      for(idx in 1:ncol(x)){
        for(jdx in 1:nrow(x)){
          ##If the data are not the same, so set null to solved matrix.
          if(x[jdx,idx] != y[jdx,idx]){
            s <<- NULL
            break
          }
        }
        ##If it is null break the for loop
        if(is.null(s))
          break
      }
      
    }else{
      s <<- NULL
    }
    
    x <<- y
  }
  get <- function() x
  setSolve <- function(sol) s <<- sol
  getSolve <- function() s
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Verify if it needs to solve the matrix or get the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  message(x$hasChanged)
  if(!is.null(s)){
    message("Getting cached data");
    return(s);
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
