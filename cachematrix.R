## The below two functions are written to compute and cache the inverse of a matrix, so it will not have to be repeatedly computed.

## The makeCacheMatrix function creates a special matrix. The function stores a list of functions to set and get the value of the matrix and to set and get the square inverse of the matrix.
## The <<- operator is used because then the enclosing (parent) environment will be searched for this variable as well, as opposed to just the current one.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special matrix that was created above.
## First it checks if the inverse has already been created (!is.null). If it has,it gets that inverse from the cache and does not compute it again. It will return the below message and the inverse.
## If the inverse has not already been created, it computes it then in the cache through the setinverse function and returns it.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
