## This pair of functions will cache the inverse of a matrix
## If the content of the matrix has not been changed, the functions can look up
## the inverse  of the matrix in the cache and return it directly when we need
## it rather than recompute the result.
## If new content of a matrix is given, the functions will calculate the new inverse
## and store it in the cache.

## The "makeCacheMatrix" function creates an object containing a list of functions
## to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse
## And the object created by the function can be used by downstream R code.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The "cacheSolve" function calculates the matrix inverse of an object 
## created by "makeCacheMatrix" function.
## First it checks if the inverse has already been calculated.
## If so, the function will skip the calculation and take the inverse from cache.
## Otherwise, it will calculate the inverse and set the value of the inverse in
## the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  b<-matrix(c(1,0,0,1),nrow = 2,ncol = 2)
  inv<-x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,b,...)
  x$setinverse(inv)
  inv
}
