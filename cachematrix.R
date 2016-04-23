## I have based on an example makeVector and cachemean from README.md
# The goal of the functions is to calculate inverse of a matrix and cache it for future calculations

# makeCacheMatrix stores a list of four functions that are available through a subset of the main function

makeCacheMatrix <- function (x=matrix()) {
  inversion <- NULL
  
  set <- function(y) {
    x <<- y #Setting a new data matrix
    inversion <<- NULL #Clearing inversion matrix when a new data matrix is set
  }
  
  get <- function() x #It returns matrix x stored in the main function
  
  setinv <- function(z) inversion <<- z #Changes the matrix (inverted) in the main function
  
  getinv <- function() inversion #It returns an inverted matrix inversion
  
  # To store all functions by assigning them to an object as a list
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Creating an inverted matrix or calling a cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inversion <- x$getinv() #Calling a subfunction that returns an inverted matrix, if it exsists
  
  if (!is.null(inversion)) { #Checking if inversion matrix is not NULL 
    message("Getting cache") #Printing a message
    return(inversion) #Return cached data and stop the function
  }
  
  locmatrix <- x$get()  #Getting data from a subfunction and stroring them in a local matrix locmatrix
  
  inversion <- solve(locmatrix, ...) #running solve function with locmatrix, stroring data in inversion matrix
  
  x$setinv(inversion) #setting global martrix inversion with locally acquired data
  
  inversion #returning / printing an inverted matrix
}

#Running a code

#locmatrix2<-matrix(12:15,ncol=2, nrow=2) #creating a matrix
#a<- makeCacheMatrix(locmatrix2) #calling a function setting an environment matrix
#a$get() #checking if a matrix is stored in "a" environment (printing data)
#a$getinv() #checking if an inverted matrix is NULL
#cacheSolve (a) #Creating an inverted matrix
#cacheSolve (a) #Calling a cache