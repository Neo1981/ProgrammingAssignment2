## The two functions below work to compute the inverse of a function.  
## To save time, inverses that are computed for the first time are cached
## Whenenver a matrix is passed to the functions to compute, 
## the cacheSolve matrix will verify the answer is not yet cached

##4 functions to set, get, set inverse, and get inverse

makeCacheMatrix <- function(m = matrix()) {
  inv = NULL
  set = function(y) {
    m <<- y
    inv <<- NULL
  }
  get = function() m
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function will compute the inverse of a matrix, first checking to see if the solution is cached

cacheSolve <- function(x, ...) {
   inv = x$getinv()
   if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

