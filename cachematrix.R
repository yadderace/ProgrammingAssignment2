## Put comments here that give an overall description of what your
## functions do

## Crea a matrix and save the original and inverse matrix, return a list of functions

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  changed <- FALSE
  set <- function(y) {
    if(!is.null(x) && is.matrix(y) && ncol(x) == ncol(y) && nrow(x) == nrow(y)){
      
      for(idx in 1:ncol(x)){
        for(jdx in 1:nrow(x)){
          if(x[jdx,idx] != y[jdx,idx]){
            s <<- NULL
            break
          }
        }
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
