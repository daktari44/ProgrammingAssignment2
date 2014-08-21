## Caching the Inverse of a Matrix

## These functions may be used to calculate and cache the inverse of a matrix reduce the computation
## time and cost required when the result of the calculation will be used repeatedly in an analysis

## This first function creates a list containing functions to 
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## The changes made using this function will affect the values of the all the named objects in the 
## global environment

##  The argument passed into this makeCacheMatrix function is the matrix whose inverse we would like to solve

makeCacheMatrix <- function(x = matrix()) {
          ## v is our inverse and is reset to NULL each time the function is called
      v <- NULL                                     
      
         ## set the value of the matrix and the inverse in the global environment
      set <- function(y){
            x <<- y
            v <<- NULL
      }
         
      get <- function(){x}                            ## returns the value of the original vector
      setinverse <- function(inverse){v <<- inverse}  ## stores the value of inverse in the global environment
      getinverse <- function(){v}                     ## returns the cached value to cacheMatrix when called
      
           ## return a list of the newly created functions as the main output of the makeCacheMatrix function
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of the matrix created in the first function.
## The argument passed into this function is the makeCacheMatrix function (or rather the object 
## returned from calling the makeCacheMatrix function)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      v <- x$getinverse()
        
        ## test to see if the inverse is already cached and return the cached value
      if(!is.null(v)){                      
            message("getting cached data")
            return(v)
      }
        ## if no cached inverse, calculate the inverse, store and return the value
      data <- x$get()
      v <- solve(data,...)
      x$setinverse(v)
      v
}
