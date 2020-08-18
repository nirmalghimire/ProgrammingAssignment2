## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(a){
    x<<-a
    inv<<-NULL
}
get<-function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function(){
  flip<-ginv(x)
  flip%*%x
}
list(set=set, get=get,
     setinv=setinv,
     getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv<-x$getinv()
      if(!is.null(inv)){## Return a matrix that is the inverse of 'x'
      message("Getting chched data!")
        return(inv)
      }
      data<-x$get()
      inv<-solve(data,...)
      x$setinv(inv)
      inv
}
f<-makeCacheMatrix(matrix(1:9, 3,3))
f$get()
cacheSolve(f)