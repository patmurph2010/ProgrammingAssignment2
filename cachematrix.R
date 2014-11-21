## These functions initialize a matrix, x, and an inverse matrix, invmat, then
## they define 4 functions, setmat, getmat, setinv, getinv, and return a list of
## these functions. This object can then be used to check if there is already an 
## inverse to the matrix and return it, or, if it does not exist, get a new
## inverse.

## makeCacheMatrix simply initializes a matrix, x, and an inverse matrix, invmat.
## It then defines 4 functions, setmat, getmat, setinv, and getinv. The return
## of this function is a list containing the 4 functions.

makeCacheMatrix <-  function(x = matrix()) {
  invmat <- NULL
  setmat <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse_matrix) invmat <<- inverse_matrix
  getinv <- function() invmat
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is a function that searches for the inverse of the matrix defined
## in makeCacheMatrix which was placed in getinv. If there is an inverse, the
## function sends the message "getting cached data" and returns the inverse 
## matrix. If not, it takes the initialized matrix from the getmat function,
## solves to find its inverse, reassigns it to the setinv function in 
## makeCacheMatrix and then returns the new inverse matrix.

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$getmat()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
