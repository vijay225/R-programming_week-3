## The following two functions, take a matrix, stores the inverse in cache.

## This function creates an inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv<-NULL
  set<-function(y){
    x<<-y
    mat_inv<<-NULL
  }
get<-function()x
setinv<-function(inverse)mat_inv<<-inverse
getinv<-function()mat_inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function tries to get the inverse of an input matrix from cache if already
## available.If not, the function calculates the inverse and outputs.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv<-x$getinv()
  if(!is.null(mat_inv)){
    message("getting cache data")
    return(mat_inv)
  }
  mat_data<-x$get()
  mat_inv<-solve(mat_data,...)
  x$setinv(mat_inv)
  mat_inv
}
