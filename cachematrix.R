## In this programme the inverse of the given matrix is calculated using cache

##This function stores the matrix and returns the list of function and caches the inverse

makeCacheMatrix <- function(x = matrix())
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,  setinverse = setinverse,getinverse = getinverse)
  
}


## This function checks wheather the inverse of the matrix is already present in cache or yet to be calculated if so it calculates the inverse and store it in cache or it just return the inverse matrix present in cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  d <- x$get()
  i <- solve(d, ...)
  x$setinverse(i)
  i
}