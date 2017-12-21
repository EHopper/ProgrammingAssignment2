## This pair of functions will allow the inverse
## of a square matrix to be cached.

## This function generates a list of functions which
## will set & get the value of the matrix, and 
## set & get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinvM <- function(solve) invM <<- solve
  getinvM <- function() invM
  list(set = set, get = get,
       setinvM = setinvM, getinvM = getinvM)
}


## This function calculates the inverse of the matrix
## (unless it is already cached, in which case it uses that)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invM <- x$getinvM()
  if(!is.null(invM)){
    message("Getting cached data!")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinvM(invM)
  invM
}
