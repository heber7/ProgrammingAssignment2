## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
  # sets 'x' equal to an empty matrix
  xirtam <- NULL
  # Set inverse 'xirtam = word mirror of word matrix' equal to NULL 
  set <- function(y) {
    x <<- y
    xirtam <<- NULL   ## Inverse is re-set to NULL
  }
  get <- function() x  ## function to return the matrix
  setInverse <- function(inverse) xirtam <<-inverse  ## overide the previous value
  getInverse <- function() xirtam  ## returns the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xirtam <- x$getInverse()       ## Retrives the recent value for the inverse
  if(!is.null(xirtam)) {
    message("getting cached inversed matrix")
    return(xirtam)                      ## If the value of the inverse is not  NULL. cacheSolve return that value
    
  } 
  xirtam <- solve(x$get())
  x$setInverse(xirtam)         ## Set the inverse of the new calculate value
  xirtam 
}
