##The two functions below create an opbject that stores a (square) matrix and caches its inverse##

## The function makeCacheMatrix creates a matrix which is a list containing a function to set the value of the matrix, gets the value of the matrix, sets the value of the inverse of the matrix, and gets the value of the inverse of the matrix. ##

makeCacheMatrix=function(x=matrix()) {
  i=NULL
  set=function(y) {
    x <<-y
    i <<-NULL
  }
  get=function() x 
  setinverse=function(inverse) i <<-inverse
  getinverse=function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The function cacheSolve first checks to see if an inverse, i, has already been calculated. If so, it gets the the inverse from the cache and skips the compiutation. If not, it calculates the inverse of the matrix x and sets the value of the inverse in the cache via the setinverse function. ## 

cacheSolve <- function(x, ...) {
i=x$getinverse
  if(is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data=x$get()
  i=solve(data, ...)
  x$setinverse(i)
  i
}
