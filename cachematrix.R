## The functions within this file are used to solve the inverse of 
## a square matrix and to provide this value in a cached form to
## avoid running potetnially time consuming code more than once

## makeCacheMatrix will solve the inverse of a matrix and cache it
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get=get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve will interact with makeCacheMatrix to provide the
## inverse of a matrix, only calculating when required
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
