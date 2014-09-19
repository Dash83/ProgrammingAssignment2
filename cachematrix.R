## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will build a special matrix, an a cached matrix.
# The cached is set upon creation of the special matrix, and it's
# only updated when the inverse of the matrix is calculated if and
# only if, the original matrix has changed.
makeCacheMatrix <- function(x = matrix()) {
  #This is the cached matrix
  x_cached <- x
  inverse_cached <- NULL
  
  #Setter for the internal matrix
  set <- function(y) {
    x <<- y
  }
  
  #Getter for the internal matrix
  get <- function() x
  
  #Setter for cached matrix
  setcache <- function(y) x_cached <<- y
  
  #Getter for the cached matrix
  getcache <- function() x_cached
  
  #Setter for the inverse matrix
  setinverse <- function(y) inverse_cached <<- y
  
  #Getter for the inverse matrix
  getinverse <- function() inverse_cached
  list(set=set, get=get, setcache=setcache, getcache=getcache, 
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function calculates the inverse of a special matrix
# previously created with the makeCacheMatrix function if and only if
# the special matrix has changed since it's creation or since it's last
# inverse was calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- NULL
  
  # Helper function to compare matrices
  equal_mat <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  }
  
  if(equal_mat(x$get(), x$getcache()) && !is.null(x$getinverse())) {
    # The matrix hasn't changed, return the cached inverse matrix
    print("Matrix has not changed. Returning cached inverse")
    inverse <- x$getinverse()
  } else {
    #The matrix has changed, let's calculate the new inverse
    print("Matrix has changed or first time calculating the inverse.")
    inverse <- solve(x$get())
    x$setcache(x$get())
    x$setinverse(inverse)
  }
  inverse
}

randSqMatrix <- function(length = 2) {
  result <- NULL
  i <- 1
  while(i <= length) {
    vec <-  (rnorm(length))
    result <- cbind(result, vec)
    i <- i+1
  }
  result
}

equal_mat <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

values <- randSqMatrix(4)
spec_mat <- makeCacheMatrix(values)
spec_mat$get()
spec_mat$getcache()
spec_mat$getinverse()
equal_mat(spec_mat$get(), spec_mat$getcache())
cacheSolve(spec_mat)
equal_mat(spec_mat$get(), spec_mat$getcache())
spec_mat$get()
spec_mat$getcache()
cacheSolve(spec_mat)
spec_mat$set(randSqMatrix(4))
equal_mat(spec_mat$get(), spec_mat$getcache())
cacheSolve(spec_mat)

