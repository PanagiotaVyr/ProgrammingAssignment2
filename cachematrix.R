## Progamming assignment 2
## makeCacheMatrix() caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  set <- function(y){
    x <<- y
    inv_x <<- NULL
  }
 
  get <- function() x
  setInverse <-function(inverse) inv_x <<- inverse
  getInverse <- function() inv_x
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() returns the inverse of a matrix x
## by utilizing solve() function
## if the inverse is cached, the cached obejct will be returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getInverse()
    
    if(!is.null(inv_x)) {
      message("getting cached data")
      return(inv_x)
    }
    
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setInverse(inv_x)
    inv_x
  }
  
