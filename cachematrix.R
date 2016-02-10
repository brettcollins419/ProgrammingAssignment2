#####Caching Matrix Inverse Calculation#####
# This fucntion will store a matrix and then calculate the matrix
# inverse when called upon the first time.  Any future calls
# to the matrix inverse using the chacesolve function will
# first check the to see if the inverse has already been calculated.
# if the inverse is already calculated, the fuction will pull the value
# from the cache, otherwise it will perform the computation. Inverse
# matrix calculation assumes a square matrix and will fail otherwise


# makeCacheMatrix creates the function structure for storing the matrix
# and subsequent inverse calculation along with the get functions to 
# return the stored values
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { 
    m <<- y
    i <<- NULL
  }
  get <- function() {m}
  setinv <- function(inv) {i <<- inv}
  getinv <- function() {i}
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function checks to see if matrix inverse has been previously calculated.
# Returns the cached inverse if available, otherwise calculates inverse and
# storse result.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message('retreiving matrix inverse')
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  return(i)
}
