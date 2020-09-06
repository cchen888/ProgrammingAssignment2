## these two functions compute the inverse of a given matrix and cache the value
## if the inverse has been computed before, the matrix will be retrieved from the cache 
## as opposed to recalculating to save time

## makeCacheMatrix creates a matrix object which can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
    	inv <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(solve) inv <<- solve
  	getinverse <- function() inv
  	list(set = set, get = get,
       	setinverse = setinverse,
       	getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by the function above. If the inverse has been 
## previously calculated, this function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
    	return(inv)
  	}
  	data <- x$get()
  	inv <- solve(data, ...)
  	x$setinverse(inv)
  	inv
}
