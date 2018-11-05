
#produce a matrix for memoring the inverse of the matrix passed as argument
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    #set original matrix into cache
    x <<- y
    m <<- NULL
  }
  #return original matrix into cache
  get <- function() x
  #set the inverse of the matrix into cache
  setinverse <- function(inverse) m <<- inverse
  #get the inverse of the matrix from cache
  getinverse <- function() m
  #list of exposed methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cache manager
cacheSolve <- function(x, ...) {
  #try to get the inverse of the matrix from cache, null otherwise
  m <- x$getinverse()
  #if cache is not null the memorized data exist and return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #else request matrix and compute the inverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  #save the inverse into cache
  x$setinverse(m)
  #return the inverse
  m
}

