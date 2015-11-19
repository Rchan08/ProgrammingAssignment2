## Put comments here that give an overall description of what your
## functions do
## 

## Write a short comment describing this function
## makeCacheMatrix takes a numeric matrix and save the data contained in the matrix
## Data is cached and can be available  when needed by calling the respective arguments of the function

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set_datos<-function(y){
    x<<-y
    m<<-NULL
  }
  get_datos<-function() x
  setinv<-function(inversa) m<<-inversa
  getinv<-function() m
  list(set_datos=set_datos,
       get_datos=get_datos,
       setinv=setinv,
       getinv=getinv)
 }


## Write a short comment describing this function
## get values cached by calling makeCacheMatrix function
## Calculate the inverse of a certain matrix and cached it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get_datos()
  m<-solve(data,...)
  x$setinv(m)
  m
}
