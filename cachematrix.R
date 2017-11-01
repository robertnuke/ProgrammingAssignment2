## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## return: a list containing functions to
  ##  1. set the matrix
  ##  2. get the matrix
  ##  3. set the inverse
  ##  4. get the inverse
  ##this list is used as the input to cacheSolve()
  
   inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setInverse <- function() inver <<- solve(x) #calculate the inverse
    getInverse <- function() inver
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ##  Return a matrix that is the inverse of 'x'
  ##  if the inverse has already been calculated
  ##  get it from the cache variable and skips calculation.
  ##  otherwise, calculates the inverse
    inver<-x$getInverse()
    if (!is.null(inver)){
      Message("getting cached data")
      Return(inver)
    }
    mat<-x$get()
    inver<-solve(mat,...)
    x$setInverse(inver)
    
    Return(inver)
}
