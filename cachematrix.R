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


## cacheSolve receive a list object. A matrix is passed to cacheSolve in this object and retrievable
## by calling getinv().
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  data <- x$get()
  ## Return cached solution if avilable
  if(!is.null(m)) {
    message("getting cached solution")
    return(m)
  }
  m <- solve(data, ...)
  x$setinv(m)
  #### Return the inverse of 'x'
  m
}
