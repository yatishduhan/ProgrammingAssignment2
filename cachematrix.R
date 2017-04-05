## Sometimes to calculate the inverse of matrix can be potentially time consuming operation
## here created the functions to allow caching of the inverse of matrix 

## This function creates a special "matrix" object that can cache its inverse
## have considered to create new matrix by passing matrix properties to the set function of this makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(data = NA, nrow = 1,ncol = 1, byrow = FALSE, dimnames = NULL) {
  # creating new matrix by allowing user to set the properties leveraging the function parameters.
    x <<- matrix(data = data,nrow = nrow,ncol = ncol, byrow = byrow, dimnames = dimnames)
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,get = get,
         setinverse = setinverse, getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inversed matrix from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## to test the above function you can use the following commands:
## t <- makeCacheMatrix()
## t$set(1:4,nrow=2,ncol=2)
## t$get()
## cacheSolve(t) running first time will create the new inverse matrix
## running cacheSolve(t) second time will return the inverse matrix from cache and you wil see hte message printed on the command line that fetched from the cache