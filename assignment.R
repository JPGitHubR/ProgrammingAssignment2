##Assignment 3
## This assignment is for finding matrix inverse using functions as given below
##makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##it consists of set, get, setInverse and getInverse functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL         #initially inverse will be set as NULL
  set <- function(y){
    x <<- y
    m <<- NULL      
  }
  get <- function()x    #function to get matrix x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m    # function to get inverse of matrix x
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}
##This function computes inverse of special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){            #checks if inverse is NULL
    message("getting cached data")
    return(m)                 # returns value of inverse
  }
  data <- x$get()
  m <- solve(data,...)        # function"solve" calcualtes inverse
  x$setInverse(m)
  m                           #returns inverse of matrix x
}

