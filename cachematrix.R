## First function (makeCacheMatrix) creates a matrix. In fact function returns a list
## of four functions:
##        1. set(): set a values of a matrix
##        2. get(): get a values of a matrix
##        3. setsolve(): set a values of a inverse matrix
##        4. getsolve(): get a values of a inverse matrix

## To comppute the inverse matrix I used solve() function 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function (cacheSolve) computes inverse matrix of that returned by the first
## function. 
## If the inverse matrix have already been calculated, the message is returned.
## The result of cacheSolve() function is matrix inversed to 'x'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
