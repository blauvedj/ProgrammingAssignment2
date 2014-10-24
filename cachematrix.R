## The following functions store a value in a 'superfunction' then perform an operation 
## in this case the inverse (solve()) function.

## This first function creates a special 'matrix' which sets 
## the value of the matrix and gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## The second function calculates the inverse of the matrix created in the first
## function. The solve() command creates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}