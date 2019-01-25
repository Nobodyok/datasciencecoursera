## The program uses two function where they cache the
## inverse of the matrix the user put in them

## "makeCacheMatrix creates a special matrix and it
## will be used to cache the inputed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##"cacheSolve" computes the cache and invert the matrix thru the solve function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) 
  x$setinv(i)
  i
}

## Return a matrix that is the inverse of 'x'
