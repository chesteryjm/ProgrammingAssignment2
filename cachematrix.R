makeMatrix <- function(x = matrix()){
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheinverse <- function(x, ...){
  
  m <- x$getinverse()
  if(!is.null(m)){
      message("Getting cache data...")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}