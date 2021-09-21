#The codes below are applied to generate a specific variable that can 
#store matrices and its respective inverse

#The function createCacheMatrix will begin the code 


createCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The function above solves the inverse of the specific matrix created 
#by the function createCacheMatrix


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

