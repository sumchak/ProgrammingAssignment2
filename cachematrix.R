## Code consists of two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix defines get and set functions to get and set values of the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	set_inverse <- function(i) inverse <<- i

	get_inverse <- function() inverse
	
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve returns cached inverse of input matrix object if it exists
## If the inverse doesn't exist, the function calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inverse <- x$get_inverse()

	if(!is.null(inverse)){
		message('Inverse has a stored value. Retrieving...')
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data)
	x$set_inverse(inverse)
	
	return(inverse)
}
