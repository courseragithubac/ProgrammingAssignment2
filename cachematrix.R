
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  
  setinversemean <- function(mean) m <<- mean
  getinversemean <- function() m
  list(set = set, get = get, setinversemean = setinversemean, getinversemean = getinversemean)
}


cacheSolve <- function(x, ...) {

  inv <- x$getinversemean()
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversemean(m)
  m      
}