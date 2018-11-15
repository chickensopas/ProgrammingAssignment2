## 

##makeCacheMatrix "creates a special "matrix" object that can cache its inverse"
makeCacheMatrix <- function(x = matrix()) {
  
  ##cache for the matrix inverse
  inv <- NULL
  
  ##for changing values without initializing another instance
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(INV) inv <<- INV
  getinv <- function() inv
  
  ##list format allows use of $ operator
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##CacheSolve "computes the inverse of the special "matrix" returned by makeCacheMatrix."
##if the inverse has already been calculated, it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  ##checks if a valid, cached inverse exists, and returns it if present
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ##computing the matrix inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

