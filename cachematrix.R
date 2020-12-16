
## Create object to store matrix

makeCacheMatrix <- function(x = matrix()) {
  {
    A_1 <- NULL
    set <- function(y) {
      x <<- y
      A_1 <<- NULL
    }
    get <- function() x
    setinv <- function(inv) A_1 <<- inv
    getinv <- function() A_1
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  
  
}


## Return the inverse of the matrix stored in the makeCacheMatrix created object, in case the inverse of matrix
#is already stored in the cache, return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  A_1 <- x$getinv()
  if(!is.null(A_1)) {
    message("getting cached data")
    return(A_1)
  }
  data <- x$get()
  A_1 <- solve(data, ...)
  x$setinv(A_1)
  A_1
}

