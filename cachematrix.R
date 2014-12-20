## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Cached inverse
	inverse <- NULL

	## Set new matrix object
	set <- function(matrix) {
		x <<- matrix
		inverse <<- NULL
	}

	## Get the actual matrix object
	get <- function() {
		x
	}

	## Store the inverse into the cache
	setInverse <- function(inv) {
		inverse <<- inv
	}

	## Get the cached inverse
	getInverse <- function() {
		inverse
	}

	## Make a list of object methods
	list(set = set, get = get,
	     setInverse = setInverse, getInverse = getInverse)	
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the inverse will be retrieved from
## the cache.

cacheSolve <- function(x, ...) {
	## See if we have something in the cache
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message("cacheSolve: Inverse from cache")
		return(inverse)
	}

	## No inverse stored in the cache, so determine inverse and
	## store it in the cache before returning itmac
	message("cacheSolve: Calculating inverse and storing in cache")
	inverse <- solve(x$get(), ...)
	x$setInverse(inverse)
	return(inverse)
}


## Some tests

test <- function() {
	m = rbind(c(1, -1/4), c(-1/4, 1))
	message("Matrix m = ")
	print(m)
	message("\n")
	cm = makeCacheMatrix(m)
	inv = cacheSolve(cm)
	message("Inverse (1 - calculated) of m = ")
	print(inv)
	message("\n")
	inv = cacheSolve(cm)
	message("Inverse (2 - from cache) of m = ")
	print(inv)
	message("\nChecking by multiplying: inv(m) * m =")
	print(inv %*% m)
	
	message("\n\n---------------\n\n")
	m = rbind(c(1, 2, 4), c(0, 7, 3), c(9, -1, -2))
	message("Matrix m = ")
	print(m)
	message("\n")
	cm = makeCacheMatrix(m)
	inv = cacheSolve(cm)
	message("Inverse (1 - calculated) of m = ")
	print(inv)
	message("\n")
	inv = cacheSolve(cm)
	message("Inverse (2 - from cache) of m = ")
	print(inv)
	message("\nChecking by multiplying: inv(m) * m =")
	print(inv %*% m)	
}
