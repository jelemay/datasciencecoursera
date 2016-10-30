## Put comments here that give an overall description of what your
## functions do

# The functions find the inverse of a matrix and cache the matrix value on first access.
# Subsequent accesses use the cached value.

## Write a short comment describing this function

# makeCacheMatrix is an object that caches the inverse of a matrix.
# It provides settrs and gettrs for the input and the cached matix object.

makeCacheMatrix <- function(x = matrix()) {
  mtxinv <- NULL
  set <- function(y) {
    x <<- y
    mtxinv <<- NULL
  }
  get <- function() x
  setmtxinv <- function(matr) mtxinv <<- matr
  getmtxinv <- function() mtxinv
  list(set = set, get = get,
       setmtxinv = setmtxinv,
       getmtxinv = getmtxinv)
  
}


## Write a short comment describing this function

# cacheSolve takes a CacheMatrix object and returns the inverse of the input matrix.
# If the inverse was cached previously it returns the cached value.
# If the input matrix is not invertible it return NULL.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmtxinv()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  if(det(data)==0){
    message("--matrix is not invertible-- ")
    return(NULL)
  }
  m <- solve(data)
  x$setmtxinv(m)
  m
}