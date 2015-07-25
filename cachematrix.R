## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setmat<-function(y){
    x<<-y
    m<<-NULL
  }
  getmat<-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=setmat,get=getmat,getinv=getinv,setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("Getting cached result")
    return(m)
  }     ## Return a matrix that is the inverse of 'x'
  matriz<-x$get()
  m<-solve(matriz, ...)
  x$setinv(m)
}
