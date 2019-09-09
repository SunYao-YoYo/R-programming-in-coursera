## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


##Testing my function
matrix <- makeCacheMatrix(matrix(1:4,2,2))
matrix$get()
matrix$getInverse()
cacheSolve(matrix)

matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
matrix$get()
matrix$getInverse()
cacheSolve(matrix)
