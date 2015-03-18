## Function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y){
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinv <- function(inv) invx <<- solve(x)
	getinv <- function() invx
	list(set = set, get = get, 
		setinv = setinv,
		getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(m)) {
		message("getting cached inverse matrix")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
