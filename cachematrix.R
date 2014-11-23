## Computing the inverse of a square matrix and cashing its value
## After first calculation, cashed value is used as long as matrix stays the same

## Function creates special matrix objects, which stores the matrix itself,
## cached inversed matrix and 4 functions to get and set matrix and to get and set inverted matrix

makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function tries to get the inverse matrix from cashe, 
## and only calculates it new if cache is empty

cacheSolve <- function(m, ...) {
  ## Return a matrix mi that is the inverse of m
  mi <- m$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- m$get()
  mi <- solve(data, ...)
  m$setinverse(mi)
  mi
}
