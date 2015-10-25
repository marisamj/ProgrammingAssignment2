##======================================================================================================================
## These functions have the purpose to optimize the necessary time to compute the inverse matrix of a given matrix.
## To do that, they will use objects to be defined and retrieved, in an environment different from the current 
## environment.
## Once a matrix has its inverse computed, it won't be necessary compute the inverse again.
##======================================================================================================================

##----------------------------------------------------------------------------------------------------------------------
## Function's name: makeCacheMatrix
##
## Purpose........: Create a special "vector", which contains a function list: 
##                  1) Function "set": set the value of a given matrix.
##                  2) Function "get": get the value of the matrix, previously stored by the function "set".
##                  3) Function "setsolve": set the value of the inverse matrix of the given matrix.
##                  4) Function "getsolve": get the value of the inverse matrix, previously stored by the function 
##                     "setsolve".
##
## Creation date..: 25/10/2015
##----------------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
  ## Assign NULL to an Object in a current environment, which will work as an auxlliary content in the definition of the 
  ## function "getsolve".
  inv_x <- NULL
  
  ## Definition of the function "set".
  set <- function(y) 
  {
    ## Assign the value of a given matrix to an object named "x" in an environment that is different from the current 
    ## environment.
    x <<- y
    ## Assign the NULL value to an object named "inv_x" in an environment that is different from the current environment.
    ## When you are setting the matrix x" you do not know the inverse of "x" yet. That's why the value of "inv_x" is 
    ## equal NULL.
    inv_x <<- NULL
  }
  
  ## Definition of the function "get": retrieve the value of the matrix previously stored by the function "set".
  get <- function() x
  
  ## Definition of the function "setsolve": assign the value of the inverse matrix of a given matrix to an object named 
  ## "inv_x" in an environment that is different from the current. 
  setsolve <- function(solve) inv_x <<- solve
  
  ## Definition of the function "getsolve": retrieve the value of the inverse matrix, previously stored by the function 
  ## "setsolve".
  getsolve <- function() inv_x
  
  ## List of the functions
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##----------------------------------------------------------------------------------------------------------------------
## Function's name: cacheSolve
##
## Purpose........: When the informed matrix hasn't had its inverse matrix solved, this function will compute its 
##                  inverse and set it in an object in an environment that is different from the current, using the 
##                  "setsolve" function (created by "makeCacheMatrix" function). 
##                  Otherwise, this function will retrieve the inverse matrix from an environment that is different from 
##                  the current environment, using the "getsolve" function  (created by "makeCacheMatrix" function). 
##
## Creation date..: 25/10/2015
##----------------------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
  ## Retrieve the inverse of a given matrix, possibly stored previously.
  inv_x <- x$getsolve()
  
  ## Check the content retrieved in the previous statement. When it is not NULL, it means that the given matrix was 
  ## treated before by this function and its inverse was stored. Knowing that, the function won't need to recompute the 
  ## inverse. 
  if(!is.null(inv_x)) 
  {
    ## Let the user know that the matrix will not be recomputed.
    message("Getting cached matrix")
    ## Return the inverse matrix previously stored and exit the function.
    return(inv_x)
  }
  
  ## Retrieve the given matrix, stored previously.
  data <- x$get()
  
  ## Compute the inverse of the retrieved matrix.
  inv_x <- solve(data, ...)
  
  ## Set the inverse of the matrix in an object in an environment that is different from the current environment, for 
  ## future use.
  x$setsolve(inv_x)
  
  ## Return a matrix that is the inverse of the given matrix.
  inv_x
}
  