##=================================================
## Assignment: Caching the Inverse of a Matrix
##=================================================
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Below written is a pair of functions that cache the inverse of a matrix.

##================================================================================================
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##================================================================================================
makeCacheMatrix <- function(x = matrix()) {
  ## First we need to initialize the global env "matrix cache" variable representing the inverse
  matrix_cache <- NULL
  
  ## Function ("set") below will "set" the matrix from  value "y" passed to global env "x"
  set<-function(y){
    x<<-y
    matrix_cache <<- NULL
  }
  
  ## Function ("get") below will "get" the matrix "x" and returns "matrix" to calling function
  get <- function() {
    ## Return the matrix
    x
  }
  ## Function ("setInverse") below will "set the inverse" of matrix and stores in "matrix_cache"
  setInverse <- function(inverse) {
    matrix_cache <<- inverse
  }
  ## Function ("getInverse") below will "get the inverse" of matrix from the "matrix_cache"
  getInverse <- function() {
    ## Return the inverse property
    matrix_cache
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##================================================================================================
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by
## "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.
##================================================================================================
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' by calling "getInverse" function
  ## Remember this function returns just the value from "matrix_cache" as per above function
  matrix_cache <- x$getInverse()
  
  ## Return the inverse if its already set
  ## Initially since the "matrix_cache" is NULL , it will return NULL first time, thus below
  ## function skipped for first time run , second time run , "matrix_cache" will indeed 
  ## have the true inverse from cache;
  if( !is.null(matrix_cache) ) {
    message("getting cached data")
    return(matrix_cache)
  }
  
  ## Get the matrix from our object
  ## For the first time run , this will get executed that gets the matrix "x" using matrix()
  ## Now variable "data" holds the actual "matrix"
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  ## Matrix inversion is a complex process and below function does it through "solve" function
  ## Now after execution of the below function variable the original matrix contained 
  ## in variable "data" is transposed and inversed and "inverse of the Matrix" now stored
  ## in variable "matrix_cache"
  matrix_cache <- solve(data, ...)
  
  
  ## Set the inverse to the object
  ## The below function calls above cache function to set the inverse of matrix to "x"
  ## This function stores the inverse to the "matrix_cache" global environment that
  ## can be used in the next run directly
  x$setInverse(matrix_cache)
  
  ## Return the matrix
  matrix_cache        
}


##====================================================================
## SOLUTION TESTING - 
## PLEASE RUN THE ABOVE FUNCTION FIRST
## TESTING THE ABOVE FUNCTION WITH SAMPLE VALUES
## PLEASE TYPE IN THE FOLLOWING TO TEST THE RESULTS
##====================================================================
# UNCOMMENT AND RUN THE COMMAND AS BELOW
##====================================================================
#my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#my_matrix$get()
#cacheSolve(my_matrix)
#cacheSolve(my_matrix)
#my_matrix$getInverse()
#my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
#my_matrix$get()
#my_matrix$getInverse()
#cacheSolve(my_matrix)
#cacheSolve(my_matrix)
#my_matrix$getInverse()