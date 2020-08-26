## This function is going to create a matrix for cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z){
        x <<- z
        inv <<- NULL
        }
  get<- function(){x}
  setInverse <- function(inver)
         {inv <<- inver}
  getInverse <- function()
         {inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  

}


##This function going to produce inverse of the given matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  if(!is.null(inv)){
     message("cached data")
     return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
