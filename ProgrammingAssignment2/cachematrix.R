
## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
## It contains the following functions:
## getMatrix           get the value of a matrix
## getInversedMatrix   get the cached inverse of the matrix value

## setMatrix           set the value of a matrix
## setInversedMatrix   push to cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache_for_inv_matrix <- NULL
  
  getMatrix <- function() {
    x
  }
  
  getInversedMatrix <- function() {
    cache_for_inv_matrix
  }
  
  setMatrix <- function(m) {
    x <<- m
    cache_for_inv_matrix <<- NULL
  }
  
  setInversedMatrix <- function(inversed_matrix) {
    cache_for_inv_matrix <<- inversed_matrix
  }
  
  list(getMatrix = getMatrix,
       getInversedMatrix = getInversedMatrix,
       setMatrix = setMatrix,
       setInversedMatrix = setInversedMatrix)
}

## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed_matrix <- x$getInversedMatrix()
  
  if (is.null(inversed_matrix)) {
    message("calculate the inverse of x and save it in cache")
    inversed_matrix <- solve(x$getMatrix(), ...)
    x$setInversedMatrix(inversed_matrix)
    return(inversed_matrix)
  }
  message("getting value from cache...")
  inversed_matrix
}
