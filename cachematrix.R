## `makeCacheMatrix()` creates a 
## a list of functions 

##1. set()-set the value of the matrix
##2. get()-get the value of the matrix
##3. setinv()-set the value of the inverse of the matrix
##4. getinv()-get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##define the list of four functions
  inv <- NULL
  
  #define set()
  set <- function(y) {
    x <<- y
   inv <<- NULL
  }
  #define get()
  get <- function() x
  #define setinv()
  setinv <- function(inv_in) inv <<- inv_in
  #define getinv()
  getinv<- function() inv
  ##create a list of the four functions and return the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##The following function 'cacheSolve()' calculates the inverse of the special matrix
##created with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it `get`s the inverse from the cache and skips the 
##computation of the inverse using Solve().Otherwise, it calculates the inverse of the
##data and sets the value of the inverse in the cache via the `setinv()` function.

cacheSolve <- function(x) {
  
      
  
  inv <- x$getinv() 
  
  ##get cached inverse if the inverse is computed
  if(!is.null(inv)) { 
    message("getting cached data") 
    return(inv)
  }
  data <- x$get()    ##extract data-matrix
  inv <- solve(data) ##compute inverse of the data
  
  ##cache the inverse and return the inverse
  x$setinv(inv) 
  inv

  
}
