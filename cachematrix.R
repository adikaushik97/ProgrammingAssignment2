## the makeCacheMatrix function creates a special "matrix" object and caches
## its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  Inverse <- NULL
  set <- function(y) 
  {
    x <<- y
    Inverse <<- NULL
  }
  get <- function(x)
  setInverse <- function(inverse) Inverse <<- inverse
  getInverse <- function() Inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## the cacheSolve function returns the inverse of the matrix created
## by the makeCacheMatrix function above. if the inverse has already been 
## calculated, the cacheSolve function skips this computation and returns the 
## inverse from the cache

cacheSolve <- function(x, ...) 
{
  Inverse <- x$getInverse()
  if(!is.null(Inverse))
  {
    message("getting cached data")
    return(Inverse)
  }
  Matrix <- x$get()
  Inverse <- solve(Matrix, ...)
  x$setInverse(Inverse)
  Inverse
}
