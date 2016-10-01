## Functions to create a matrix object that can store it inverse
## Also retrieve function to get the value from chached storage rather than computing it

## makeCacheMatrix prepare a special matrix that can
### 1. Initialize new matrix through set  
### 2. get current matrix through get  
### 3. Save inverse of matrix through saveinverse
### 4. Retrieve saved inverse matrix through getinverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # set function to initialize new matrix,inverse to null for that case
  set <- function(mx) {
    
    x <<- mx
    inv <<- NULL
    
  }
  
  # get the matrix from closure
  get <- function() x
  
  # save new inverse in enviorment
  saveinverse <- function(imx) inv <<- imx
  
  # Function to retrieve existing saved value 
  getinverse <- function() inv
  
  # prepare and return a list of functions to operate on saved matrix
  list(set=set,get=get,saveinverse=saveinverse,getinverse=getinverse)
  
}


## This function retrieve/compute the inverse of matrix object - 
## created using makeCacheMatrix 
## if already exists then its get from cache , Otherwise it - 
##computes and save the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check whether saved value exists for matrix object
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Got inverse from cache ..exiting")
    return (inv)
  
  }
  
  mat <- x$get()
  
  # Check whether matrix is invertiable otherwise show error
  if(det(mat) == 0 )
  {
    stop("Determinent is 0, Cannot find inverse")
    
  }
  
  inv <- solve(mat)
  
  x$saveinverse(inv)
  
  inv
  
}
