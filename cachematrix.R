## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {     ## when a new matrix is set
    x <<- y                ## then the new matrix is stored into the object
    cachedInverse <<- NULL ## and the calculated inverse is reset to null
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse  ## sets the cache with the calculated inverse
  getinverse <- function() cachedInverse                     ## returns the inverse previously stored into the cache 
  
  ## the new object is returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()  ## the inverse is got from the cache
  if(!is.null(inverse)) {    
    message("getting cached data")
    return(inverse)          ## if the cache is not empty its value is returned
  }
  mtrx <- x$get()       
  inverse <- solve(mtrx, ...) ## else the inverse is calculated,
  x$setinverse(inverse)       ## stored into the cache
  inverse                     ## and returned
}
