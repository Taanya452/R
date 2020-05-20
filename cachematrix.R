## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - 
## x is a square invertible matrix
## The makeCacheMatrix is a function creates a special "matrix" object that can cache the inverse.
## which is a list containing functions
## 1 set the matrix
## 2 get the matrix
## 3 set the inverse 
## 4 get the inverse
## <<- operator assigns a value to an object in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  setmatrix<- function(y){
    x<<-y
    inv<<- NULL
  }
  getmatrix<- function()x
  setinv<- function(solve) inv<<- solve
  getinv<- function() inv
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinv=setinv,getinv=getinv)
  
}

## Write a short comment describing this function 
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){                      ## if the inverse has already bin calculated then it would retrieve the inverse from cache
    message("getting cached data")
    return(inv)
  }
  data<- x$getmatrix()
  inv<- solve(data,...)                   ## Otherwise calculate the inverse from the data.
  x$setinv(inv)
  return(inv)
  ## Return a matrix that is the inverse of 'x'
}
