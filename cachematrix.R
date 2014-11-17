## These functions create and store a matrix, and return its inverse.
## 	They also cache the matrix inverse. 

## This function creates a list of functions for setting and getting the
##	value of a matrix, and setting and getting the matrix's inverse 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function checks whether the inverse has already been calculated.
##	If it has, the function retrieves the cached inverse.
##	Otherwise, the function calculates the inverse and caches it.  
## The function returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
