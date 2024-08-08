#Programming Assignment 2: Lexical Scoping
#August 7, 2024
#Author: T.W.

# The function makeCacheMatrix takes as input a matrix and returns 
# a special "matrix" to which we refer as cache-matrix. This is really a list
# containing a function to
# 1. set the values of the matrix
# 2. get the the matrix
# 3. set the inverse of the matrix and cache it
# 4. get the inverse of the matrix
#
# The default input of makeCacheMatrix is the empty matrix
#
# To define the functions described in 1. and 4. the super 
# assignment operator <<- is used to change the matrix and
# its inverse respectively



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize inv as NULL matrix
  set <- function(y) {
    x <<- y #Sets the value of the matrix
    inv  <<- NULL #resets the value of the inverse to NULL
  }
  get <- function() x #returns the matrix x
  setInv <- function(inverse) inv <<- inverse #set the value of cached inverse
  getInv <- function() inv #returns cached inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv) #returns cache matrix with four functions
}


# The function cacheSolve returns the inverse of a cache-matrix. 
# If the inverse has already been calculated, cacheSolve retrieves the inverse
# from the cache. If cacheSolve retrieves the inverse from the cache a
# message with "getting cached data" is posted.


cacheSolve <- function(x, ...) {
  y <- x$getInv()
  if(!is.null(y)) { #Checking if inverse is cached
    message("getting cached data") 
    return(y) #return cached inverse matrix
  }
  x$setInv(solve(x$get(),...)) #determine and set inverse if inverse is not cached
  y <- x$getInv() #return the inverse matrix
  y
}