## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {  ## set the value of the matrix
    x <<- y
    m <<- NULL
    }
  
  get <- function() x  ## get the value of the matrix
  setinverse <- function(inverse) m <<- inverse  ## set the value of the inverse 
  getinverse <- function() m  ## get the value of the inverse
  list(set =set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  m <- x$getinverse()  ## checks to see if the inverse has already been calculated
  if(!is.null(m)) {  ## gets the mean from the cache and skips the computation if it is calculated yet
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()  
  m <- solve(matrix, ...)
  x$setinverse(m)   ## calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
  m   ## Return a matrix that is the inverse of 'x'
}

