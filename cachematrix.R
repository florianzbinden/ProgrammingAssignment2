##  overall description of what the functions do

# blabla

## short comment describing this function

# set the matrix
# get the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix


makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



##  short comment describing this function

# get the value of the inverse of the matrix


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



# test
ss <- makeCacheMatrix()           # initialize
ss2 <- matrix(c(2,2,3,2), 2, 2)
ss <- makeCacheMatrix(ss2)
ss$get                            # get the vector (pas oblig.)
cacheSolve(ss)                    # calculate the mean
cacheSolve(ss)                    # when is called back use the cached mean  
