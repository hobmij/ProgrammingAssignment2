##  This function takes a matrix argument and creates
##    a list object which stores the matrix and is capable of caching
##    the inverse of the matrix. The purpose of the cache is to demonstrate
##    the use of lexical scope and provide a time saving method
##    for recalling the inverse matrix.

## usage:
##    > my_matrix  <- matrix(c(4, 3, 3, 2), 2, 2)
##    > my_matrix
##      [,1] [,2]
##      [1,]    4    3
##      [2,]    3    2
##    > my_mat_obj <- makeCacheMatrix(my_matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(my_inverse) m <<- my_inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function:
## cacheSolve takes a makeCacheMatrix as an argument
##     if the inverse is not cached it will
##        1. calculate the inverse via solve()
##        2. cache the inverse
##        3. and return the inverse
##     if the inverse exists it returns the inverse

## usage:
##   cacheSolve(my_mat_obj)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
