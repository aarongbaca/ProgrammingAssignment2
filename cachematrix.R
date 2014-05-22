## The makeCacheMatrix function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                     #Name function 'get' command is x
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function further below computes the inverse of the special "matrix" returned by the
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

a <- makeCacheMatrix()  #initialize
a                       # shows that a is now a list of functions
class(a)                # shows that a is a list
class(a$set)            # shows that the elements of the list are functions
a$set(matrix(1:4,2,2))  #set the matrix
a$get()                 #get the matrix 
cacheSolve(a)           #calculate for solve/inverse 
cacheSolve(a)           #when called back use the cached mean  
