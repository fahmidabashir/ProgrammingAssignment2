## The purpose of this assignment is to create two functions called makeCacheMatrix and cacheSolve
## that have multiple subfunctions. makeCacheMatrix stores or changes values of the matrix and caches
## the computation of the matrix. CacheSolve will retrieve the cached matrix from function makeCachMatrix and
## will compute inverse of matrix. 

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {    
  m <- NULL
  ## setting solve matrix to NULL as a placeholder for future values
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  ## defines function to set matrix x to new matrix y and resets solve matrix,m, to NULL
  get <- function() x
  ## gets matrix x
  setmatrix <- function(solve) m <<- solve
  ## sets solve of matrix, m
  getmatrix <- function() m
  ## gets the solve matrix, m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
  
}


## cacheSolve returns the inverse of matrix x, which was created in makeCacheMatrix function.
## if cached inverse is availble, cacheSolve gets it. If not, then cacheSolve 
## computes matrix, caches it, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
  
}
