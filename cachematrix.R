## Matrix inversion is a computing intensive task in data analysis. It could potentially speed up the
## computation if the inverse matrix is cached in the R environment. The following two functions track 
## the states of the matrix and calcuate its inverse matrix only if necessary. 

## Note: the current implementation assumes that the matrix given is always invertible. User may need
## to verify the singularity of the matrix before using these functions. 

## Example: inverse the identity matrix of dimension 3X3

## > x <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
## > cX <- makeCacheMatrix(x)
## > invX <- cacheSolve(cMt)

## ***************************************************************************
##                            makeCacheMatrix function
## ***************************************************************************
## This function creates a special "matrix" object that can cache its inverse. 
## Input: an invertable matrix 
## Return: a list that wraps the inverse matrix and a set of helper functions
## helper functions:
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
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix))
  {
    message("getting inverse matrix")
    return(invMatrix)
  }
  m <- x$get()
  invMatrix <- solve(m)
  x$setInverse(invMatrix)
  ## Return a matrix that is the inverse of 'x'
  invMatrix
}
