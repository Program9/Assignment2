## The two functions makeCacheMatrix() and cacheSolve() use the functionality of the R
## solve() function to calculate and cache the inverse of a square matrix.  When called
## with a square matrix as argument, with other parameters set to default values, the 
## solve() function returns the required inverse.



## The function makeCacheMatrix() does not calculate the matrix inverse, but rather creates
## an environment in which the matrix and its inverse are cached and their values set.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}



## Whereas makeCacheMatrix() manages the cache, this function cacheSolve() provides the user interface to 
## either retrieve the cached inverse or calculate the inverse.  This function also uses the get and
## set methods for both the matrix and its inverse to get and set arrays stored in the makeCacheMatrix
## environment. Inversion of the matrix, if required, is carried out in the cacheSolve function. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
