#makeCacheMatrix is function defined to creates a special "matrix" object that able to cache inverse matrix
makeCacheMatrix<-function(x=matrix()){
  #Defined variable matrix here 
  matr <- NULL
  #Defined function to get values x
  set <- function(y) {
    x <<- y
    matr <<- NULL
  }
  #Defined function to search values of X
  get <- function() x
  #Defined function to set inverse matrix value
  setinverse <- function(solve) matr <<- solve
  #Defined function to get inverse matrix value
  getinverse <- function() matr
  #Defined function to listing down the sub-functions under the 'makeCacheMatrix' function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve is function defined to computes the inverse of the special "matrix" obtain from makeCacheMatrix function

cacheSolve <- function(x, ...) {
  #obtain inverse matrix calculated previously 
  matr <- x$getinverse()
  #Checking for null value, if value returned is not null value,computational steps below will skip and return value of matr
  if(!is.null(matr)) {
    message("obtaining cached data")
    return(matr)
  }
  #if condition is false, proceed to below steps
  data <- x$get()
  matr <- solve(data, ...)
  x$setinverse(matr)
  matr
}