## Put comments here that give an overall description of what your
## functions do:
# makeCacheMatrix and cacheSolve implement a caching mechanism for matrix 
# inversion. makeCacheMatrix creates a special matrix object that stores 
# both a matrix and its inverse, while cacheSolve computes the inverse 
# only when necessary. If the inverse has already been computed and the 
# matrix has not changed, cacheSolve retrieves the cached inverse instead 
# of recomputing it, thereby improving computational efficiency.

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can store a matrix
# and cache its inverse. The inverse is computed only once and saved.
# If the matrix is updated, the cached inverse is cleared so it can be
# recomputed when needed. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL # reset cached inverse
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the matrix stored in the special
# "matrix" object created by makeCacheMatrix. If the inverse has already
# been computed and cached, it retrieves the cached value instead of
# recomputing it. This avoids repeated and expensive matrix inversion.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv      ## Return a matrix that is the inverse of 'x'
}
