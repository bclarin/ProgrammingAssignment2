#Caching Inverse Matrix Assignment 
#R Programming Assignment 
#April 26th, 2015

#Note that caching mean of a vector was used to complete this assignment. 

#MakeCacheMatrix (1) set the value of the matrix (2) get the value of the matrix
# (3) set the value of the inverse matrix (4) get the value of inverse matrix 

#Vector Example
#makeVector <- function(x = numeric()) {
# m <- NULL
  #set <- function(y) {
   # x <<- y
    #m <<- NULL
 
  #}  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  #Vector Example
  #get <- function() x
  #setmean <- function(mean) m <<- mean
  #getmean <- function() m
  #list(set = set, get = get,
       #setmean = setmean,
       #getmean = getmean)
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The following function calculates the inverse of the special "matrix" created with the 
#above function. However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
#calculates the inverse of the data and sets the value of the inverse in the cache via 
#the setinverse function.

#Cachemean example used 
#cachemean <- function(x, ...) {
  #m <- x$getmean()
  #if(!is.null(m)) {
    #message("getting cached data")
    #return(m)
  #}
  #data <- x$get()
  #m <- mean(data, ...)
  #x$setmean(m)
  #m
#}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

###Note the remaining was used to check program results

#NOTE: This is the visual test used for verifying completion
#m <- matrix(c(-1, -2, 1, 1), 2,2)
#x <- makeCacheMatrix(m)
#x$get()
#Answer received on the console: 
#[,1] [,2]
#[1,]   -1    1
#[2,]   -2    1

#inv <- cacheSolve(x)
#inv
#Answer received on the console: 
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1

#inv <- cacheSolve(x)
#Answer received on the console: getting cached data.

#inv
#Answer received on the console: 
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1

## Test basic caching provided in the discussion forums
##
#n <- 3
#mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
#matCached <- makeCacheMatrix(mat)
#matSolved1 <- cacheSolve(matCached)
#matSolved2 <- cacheSolve(matCached)
#if (!identical(matSolved1, matSolved2))
 # message("Cached version does not match solved version")

## Use a time test to see if we really save time provided in the discussion forum
##
#n <- 128
#mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
#matCached <- makeCacheMatrix(mat)
#time1 <- system.time(matSolved1 <- cacheSolve(matCached))
#time2 <- system.time(matSolved2 <- cacheSolve(matCached))
#if (time1["user.self"] < time2["user.self"])
  #message("Solve time is less than cache time")