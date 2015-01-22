## Matrix inversion is a computing intensive task in data analysis. Caching the inverse matrix could 
## speed up the computation for some repeated tasks. The following two functions track the states of 
## the matrix, and calculate its inverse matrix only if necessary.

## Note: these functions assume that the matrix given is always invertible. User needs to verify the 
## singularity of the matrix before using these functions. 

## Example: calculate the inversion of 3X3 identity matrix.

## > x <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
## > cX <- makeCacheMatrix(x)
## > invX <- cacheSolve(cMt)

## ***************************************************************************
##                            makeCacheMatrix function
## ***************************************************************************
## This function creates a special "matrix" object that can cache its inverse. 

## Input: an invertable matrix 
## Return: a list that wraps the inverse matrix and a set of functions
## functions:
## set(matrix):               set the matrix
## get():                     get the matrix
## setInverse(inverseMatrix): set the inverse matrix
## getInverse():              get the inverse matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverseMatrix) inv <<- inverseMatrix
  getInv <- function() inv
  list(set = set, get = get, setInverse = setInv, getInverse = getInv)
}

## ***************************************************************************
##                            cacheSolve function
## ***************************************************************************
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

## Input: an list which is created by makeCacheMatrix function
## Return: the resultant inverse matrix

cacheSolve <- function(x, ...) 
{
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix))
  {
    ## message("getting inverse matrix")
    return(invMatrix)
  }
  m <- x$get()
  invMatrix <- solve(m)
  x$setInverse(invMatrix)
  ## Return a matrix that is the inverse of 'x'
  invMatrix
}
