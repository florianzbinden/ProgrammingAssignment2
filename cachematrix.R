
## Welcome ! Below is my code for the programming assignment 2.

## the makeCacheMatrix function calculates the inverse of a matrix and caches its value

makeCacheMatrix <- function(x = numeric()) {     # argument is an empty numeric matrix (stored in function's environment). 
                                                 # This function intiates a "child" environment (while the current R session is the "parent's" environment).
  m <- NULL                                      # internal variable m is initiated and set to NULL
  set <- function(y) {                           # function with y as an argument
    x <<- y                                      # assigns the matrix into x (into parent's environment where the fonction is called)
    m <<- NULL                                   # clears the cache (into parent's environment where the fonction is called)
  }
  get <- function() x                            # subfonction : grab matrix stored in x (argument of fonction MakeCacheMatrix) and returns it
  setsolve <- function(solve) m <<- solve        # takes the inverse (of the matrix) and stores it into m (cache)
  getsolve <- function() m                       # subfunction that returns the cache when requested
  list(set = set, get = get,                     # returns the functions in a list (for later call by cacheSolve)
       setsolve = setsolve,
       getsolve = getsolve)                      
}


## this function gets the value of the inverse of the matrix and if the second matrix submitted has not changed 
   # then it returns the value from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {                              # if the value of m is not NULL, then it contains a value; 
                                                 # therefore it returns the previous value of inverse as this signifies that the matrix is the same like before
    message("getting cached data")               # it returns a message indicating that the value is stored and will be get
    return(m)                                    # and returns the value of the inverse
  }
  data <- x$get()                                # if the value of m is NULL, then it does not contain a value; 
  m <- solve(data, ...)                          # therefore it calculates the inverse of the matrix,
  x$setsolve(m)                                  # stores it for further comparison (through running of the functions)
  m                                              # and returns it 
}





# for testing the code, use the following commands in the console
ss <- makeCacheMatrix()           # initialize a matrix
ss2 <- matrix(c(2,2,3,2), 2, 2)
ss <- makeCacheMatrix(ss2)
ss$get                            # get the matrix (not mandatory)
cacheSolve(ss)                    # calculate the inverse
cacheSolve(ss)                    # when is called back use the cached inverse  
