## The makeCacheMatrix and cacheSolve functions make a cache matrix that can solve for its
## inverse and store it. The cacheSolve function will skip computing the inverse matrix if
## it has already been solved and stored.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix as input and makes a CacheMatrix. Then it stores the matrix and 
## CacheMatrix in the parent environment and makes a list so the cacheSolve function
## can reference them.

makeCacheMatrix <- function(Matrix = matrix()) {
  CacheMatrix <- NULL
  set <- function(SetMatrix) {
    Matrix <-- SetMatrix
    CacheMatrix <-- NULL 
  }
  get <- function() Matrix
  setInvM <- function(InvM) CacheMatrix <<- InvM
  getInvM <- function() CacheMatrix
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM)

}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve
## the inverse from the cache.

cacheSolve <- function(Matrix, ...) {
  CacheMatrix <- Matrix$getInvM()
  if(!is.null(CacheMatrix)) {
    message("getting cached data")
    return(CacheMatrix)
  }
  data <- Matrix$get()
  CacheMatrix <- solve(data, ...)
  Matrix$setInvM(CacheMatrix)
  CacheMatrix
}
