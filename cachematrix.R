## The first function creates a special matrix containing
## the value of the matrix, and a list of functions that 
## will execute different actions, basically, set a value
## or get a value wheter this be the matrix or its inverse.
## 
## The second function will verify first if there is a 
## inverse already calculated and if that is the case
## will retreive is content. If not, it will calculate
## the inverse and cache the calculated value using one
## of the functions of the list defined in the first 
##function.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setmat<-function(y){
    x<<-y
    m<<-NULL
  }
  getmat<-function() x 
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=setmat,get=getmat,getinv=getinv,setinv=setinv)## Creation of the list that will allow us to call the functions
}

## Function thar returns the inverse of matrix "x" 

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){ ## Verifies if there is already a value stored in m
    message("Getting cached result")## if TRUE the value is retrieved and returned
    return(m)
  }     
  matriz<-x$get()## This will get the actual value of x.
  m<-solve(matriz, ...)## Calculates the inverse of matrix x.
  x$setinv(m) ## Sets the value of the inverse matrix in cache
  m ## Returns a matrix that is the inverse of 'x'
}
