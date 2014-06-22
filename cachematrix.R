## These functions cache the inversion of a matrix because
## matrix inversion operations can be expensive.

## This function creates an 'object' that can store a
## matrix and its inversion in cache.
##
## There is a set function, to be used to initialize or
## change the value of matrix
##
## There is a get function, which returns the matrix
##
## There is a getInverted function which returns the inverse
## of the matrix.
makeCacheMatrix <- function(x = matrix()) 
{
  set <- function(y) 
  {
    ## To initialize the 'cache'
    if (is.null(cachedMatrix))
    {
      cachedMatrix <<- y
      cachedInvertedMatrix <<- NULL
    }
    ## To invalidate the 'cache'. This could be expensive.
    else if (!identical(y, cachedMatrix))
    {
      cachedMatrix <<- y
      cachedInvertedMatrix <<- NULL
    }
  }
  
  setInverted <- function(inverted)
  {
    cachedInvertedMatrix <<- inverted
  }
  
  get <- function()
  {
    cachedMatrix
  }
  
  getInverted <- function()
  {
    cachedInvertedMatrix
  }
  
  list(set = set, get = get, getInverted = getInverted, setInverted = setInverted)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
  if (is.null(x$getInverted()))?
  {
    x$setInverted(solve(x$get()))
  }
  x$getInverted()
}
