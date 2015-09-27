
##   The below functions caclulate the inverse of a matrix if it hasn't
##   been calculated before

##  The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##    The following function calculates the inverse of the special "vector"
##    created with the above function. However, it first checks to see 
##    if the inverse has already been calculated. If so, it gets the inverse 
##    from the cache and skips the computation. Otherwise, it calculates 
##    the inverse of the matrix and sets the value of the inverse in the cache 
##    via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}