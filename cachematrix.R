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
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv  <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# The function cacheSolve returns the inverse of a cache-matrix. 
# If the inverse has already been calculated, cacheSolve retrieves the inverse
# from the cache. If cacheSolve retrieves the inverse from the cache a
# message with "getting cached data" is posted.


cacheSolve <- function(x, ...) {
  y <- x$getInv()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  x$setInv(solve(x$get(),...))
  y <- x$getInv()
  y
}