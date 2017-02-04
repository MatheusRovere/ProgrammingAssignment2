## Put comments here that give an overall description of what your
## functions do


## The function makeCacheMatrix creates a matrix that can cache
## it's inverse. There are 4 functions inside makeCacheMatrix:
## set: sets the value of our new matrix
## get gets the value of our new matrix
## setmatrixinverse: this function uses the R function named "solve" to inverse the named matrix. 
## getmatrixinverse: this function gets the inverse of the matrix.
##It's important to note that setmatrixinverse and getmatrixinverse don't calculate the mean
##by themselves, but they do store the value of the input in our variable 'm'.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##We'll keep the name of our first NULL variable as M, like as in the Assignment example.
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setmatrixinverse <- function(solve) m <<- solve
  getmatrixinverse <- function() m
  list(set = set, get = get, setmatrixinverse = setmatrixinverse, getmatrixinverse = getmatrixinverse)
}



## The function cacheSolve computes the inverse of the matrix
##returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the function
##retrieves the inverse from the cache.

cacheSolve <- function(x, ...){
  m <- x$getmatrixinverse()
  if(!is.null(m)){ ##If m is different from NULL, then it returns the value of m
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## If m wasn't calculated, the variable data gets the matrix stored with makeCacheMatrix
  m <- solve(data,...) ## and m is assigned the value of the inverse of this matrix
  x$setmatrixinverse(m) #And, finally, this line of code stores it in the object m.
  m
}
