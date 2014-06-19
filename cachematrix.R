## Below are two functions that are used to create an object that stores a matrix and 
## caches its inversion. 

## The makeCacheMatrix function creates a matrix vector. Using this function you can 
## set the value of the matrix, and get the value of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}

## The cacheSolve function, calculates the inversion of the matrix assigned in the
## makeCacheMatrix function (see previous). It first checks to see if the inversion has 
## been calculated. If it has been calculated, it gets the inversion from the cache and 
## skips over the computation. If it hasn't, the inversion is calculated and sets this
## value in the cache via the setmatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  return(m)
}


