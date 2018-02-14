makeCacheMatrix <- function(x = matrix()) {
  ## X: a square invertible matrix
  ## return: a list with functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is inserted intoo cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the environment I am working in at the moment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## x: result (output) of makeCacheMatrix()
  ## return: inverse of the original matrix and the input to makeCacheMatrix()
  
  inv = x$getinv()
  
  ## if the inverse exists already, is already calculated
  if (!is.null(inv)){
    # get it from the cache and skips the calculation. 
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ## determines the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)}
