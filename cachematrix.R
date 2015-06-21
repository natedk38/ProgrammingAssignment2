##The makeCacheMatrix and cacheSolve functions will create a special matrix
##object that can cache its inverse.  If the inverse of the matrix passed has 
##already been calculated it will retrieve the inverse matrix from cache, 
##otherwise cacheSolve will calculate and return the inverse matrix



## makeCacheMatrix will take a matrix input and output a list of functions
## that can be passed to other functions. it allows for setting a new value to 
## a matrix or storing the value of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatcache <- NULL ##invmatcache is the variable to store the matrix which
                      ##we will take the inverse of in cacheSolve
  set <- function(y) {
    ## 'set' function changes the matrix that is stored in the main function
    x <<- y 
    invmatcache <<- NULL ##restores the null value to invmatchache since we don't 
                         ##need the  
  }
  get <- function() x 
  setinverse <- function(inverse) invmatchache <<- inverse
  getinverse <- function() invmatcache
  ##create a list to store the 4 functions that we need to pass to the object
  ##makeCacheMatrix gets assigned to
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix, computes the inverse of the matrix 
## passed by makeCacheMatrix (x), and stores that in the variable 'invmat'.  
## If 'invmat' has been filled already (i.e. is not null), then cacheSolve will
## print a message notifiying the user that the data is being retreived from cache and 
## then it will return the cached value of 'invmat'
## If 'invmat' is empty/null, then it will take 'x' and calculate then return the
## inverse

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  if(!is.null(invmat)) {
    ## 'if' statement checks to see if 'invmat' is already filled or null and returns
    ## the stored matrix
    message("getting cached data")
    return(invmat) 
  }
  ## if 'invmat' is empty/null the following 'else' loop will grab the desired matrix
  ## from the main function, compute it's inverse, and return the inverted matrix
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}