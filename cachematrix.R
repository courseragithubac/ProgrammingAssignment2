
# this function creates matrix
makeCacheMatrix <- function(x = matrix()) {
  #set the value of matrix.
  # <<- operator is used to assign value to an object
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #get the value of function
  get <- function() x
  
  #set the inverse
  setinversemean <- function(inverse) m <<- inverse
  getinversemean <- function() m
  list(set = set, get = get, setinversemean = setinversemean, getinversemean = getinversemean)
}


cacheSolve <- function(x, ...) {
#input return value of makeCacheMatrix
#find inverse  
  inv <- x$getinversemean()
  
#if inverse is found then return value.
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  # set the value of inverse
  x$setinversemean(m)
  m      
}