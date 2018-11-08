#function to store cache matrix

makeCacheMatrix <- function(x = matrix()){

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

#function that computes the inverse
#if the inverse have already been calculated, the function will retrieve the inverse from the cache
cacheSolve <- function(x, ...){
  
  m <- x$getinverse()    #if there is already an inverse in the cache, the function will return the inverse stored in cache
  if(!is.null(m)){
      message("Getting cache data...")
      return(m)
  }
  data <- x$get()        #if there is no inverse in the cache, the function will calculate the inverse and store the data in the cache matrix
  m <- solve(data,...)
  x$setinverse(m)
  m
}