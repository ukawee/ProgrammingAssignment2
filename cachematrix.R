## Assignment 2: solve/cachce matrix
## Please use example command below for execution
# a = c(1,2,3,4)
# mat = matrix(a,2,2)
# mat1<-makeMatrix(mat)
# cacheSolve(mat1)

## make Cache matrix function. 
# Please use example command below for execution
# a = c(1,2,3,4)
# mat = matrix(a,2,2)
# mat1<-makeMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  # a = c(1,2,3,4)
  # mat = matrix(a,2,2)
  # mat1<-makeMatrix(mat)
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Solve for inverse matrix if no cachce (if there is a cache, use a cache)
# Please use example command below for execution
# cacheSolve(mat1)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat2 <- x$get()
  inv <- solve(mat2, ...)
  x$setInv(inv)
  inv
}
