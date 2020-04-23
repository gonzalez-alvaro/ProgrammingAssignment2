## Overall description of what the functions do

# The function makeCacheMatrix creates a cache object to store the 
# inverse of a matrix.

# The function cacheSolve updates the  inverse of the matrix sent by 
# makeCacheMatrix. If the value has not changed, then it retrieves it 
# from the cache without doing the calculation.

##  HOW TO TEST THIS CODE?
# 1. Create an invertible matrix A
# 2. Convert matrix A into an object of type "makeCacheMatrix()" by using the following code: myMatrix<-makeCacheMatrix(A)
# 3. Calculate the inverse of matrix A with: cacheSolve(myMatrix)
# 4. Verify that the inverse was stored (chached) by repeating the command "cacheSolve(myMatrix)". You will get the message "Getting cached data" followed by the value of the inverse. This means that the inverse was succesfully retrieved without being calculated once again.
# 5. Create a new invertible matrix and A go through steps 2-3. You will notice now that since matrix A has been change its inverse must be calculated and the cached message won't be displayed.


## Fucntion makeCacheMatrix(): this function creates a new object type from an invertible matrix. In this object not only the value of the matrix is stored, but also its currently calculated inverse and functions that allow to access and modify its value.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse
  setinv <- function(solve) m <<- solve
  
  # Get the value of the inverse
  getinv <- function() m
  
  # Naming the functions as list elements. This allows to access them with the "$" extractor operator
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get, # gives the name 'get' to the get() function defined above
       
       setinv = setinv,  # gives the name 'setmean' to the setmean() function defined above
       getinv = getinv)  # gives the name 'getmean' to the getmean() function defined above
}


## Function cacheSolve(): this function checks whether the inverse of the passed matrix has been previously calculated, if so it return its value with a flag message. If it has not been previously calculated, it calculates it and return its value.
# cacheSolve only works when the matrix "x" is of type makeCacheMatrix(), i.e., it does not work for normal matrices.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # We first retreive the value of m, i.e., the cache
  m <- x$getinv()
  
  # If it is not empty, we returned the stored value
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  # If it the cache is empty, then we calaculate the inverse and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
