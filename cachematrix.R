makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # sets default value for m
  y <- NULL #sets default value for y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x) # make inverse matrix
  getinverse <- function() m # cache inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # get cached matrix
  if(!is.null(m)) { #check to see if cached value exists
    message("getting cached data") #
    return(m) # return cached matrix if it exists
  }
  y <- x$get(m)
  x$set()
  m <- solve(x) # run inverse if cached matrix doesn't exist
  m
}
