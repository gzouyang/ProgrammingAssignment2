## makeCacheMatrix is a constructor function that takes a Matrix as its only input variable. 
## The function creates a list of four functions (set,get,setinv,getinv). cacheSolve function use
## a list created with makeCacheMatrix to calculate the inverse of the Matrix. 

## 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve use getinv(), get() for retrieving last inverse solution and the matrix.
## A copy of the matrix is saved to globalEnv for later comparing if matrix has changed or not.
## If the matrix is identical to last processed matrix (lastMatrix), cacheSolve will return
## last inverse result, which was saved in the list object. Otherwise resolve() is called and
## a inversed matrix is saved in the list object and subsequently returned.
##
## Should matrix is changed between two cacheSolve() runs, it will know and re-process resolve().
## This is possible because lastMatrix is in globalEnv hence not subject to change of
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##First check to see if the matrix has changed or not
  m <- x$getinv()
  ## message(paste("get() has class:", class(x$get()))), showing get() is for passing data
  data <- x$get()
  ## Save a copy of last processed data for later comparing
  lastMatrix <- data
  if(!is.null(m) & identical(data,lastMatrix)) {
    message("getting cached solution")
    return(m)
  }
  m <- solve(data, ...)
  x$setinv(m)
  m
}
