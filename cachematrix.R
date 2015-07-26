## R-Programming -> Programming Assignment 2 -> CacheMatrix

## Calculating the inverse of a matrix can be computationaly expensive 
## and the inverse is often used multiple times. These two functions 
## define a structure that combines the matrix with the possibility to 
## cache its inverse

## makeCacheMatrix 
## This function creates the special cached matrix and returns functions
## to retrieve and manipulate its values

## cacheSolve 
## This (helper) function can be used to retrieve the inverse of a matrix
## that was created using makeCacheMatrix. It will test if the inverse
## was already stored and will either:
##   If a cached value is available:
##   a) Return the value if the inverse was stored/cached
##   or otherwise:
##   b) Calculate the inverse, store/cache the inverse and return it.

makeCacheMatrix <- function(matrix = matrix()) {
	# Initialize inverse to NULL
	matrix.inverse <- NULL

	# Set value of matrix to new value
	# Clear cached inverse
	set <- function(value){
		matrix <<- value
		matrix.inverse <<- NULL
	}

	# Retrieve matrix
	get <- function() {
		matrix
	}

	# Set the inverse of the matrix
	setinverse <- function(value){
		matrix.inverse <<- value
	}

	# Retrieve the inverse of the matrix
	getinverse <- function(){
		matrix.inverse
	}

	# Return list with all functions
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}

cacheSolve <- function(cache.matrix, ...) {
	# Retreive stored matrix inverse
	matrix.inverse <- cache.matrix$getinverse()

	# Determine if a cached matrix inverse was set
	if (is.null(matrix.inverse)){
		# Matrix inverse was not set so calculate value
		
		# Retrieve matrix
		matrix <- cache.matrix$get()

		# Calculate matrix inverse
		matrix.inverse <- solve(matrix, ...)

		# Store the matrix inverse
		cache.matrix$setinverse(matrix.inverse)
	} 
	
	# Return the matrix
	matrix.inverse	
}

## Example usage:

## > example.matrix <- matrix(c(0,0,1,0,1,1,1,1,1), ncol=3, nrow=3)
## > cache.matrix <- makeCacheMatrix(example.matrix)
## > cache.matrix$getinverse()	
## NULL
## > cacheSolve(cache.matrix)
##      [,1] [,2] [,3]
## [1,]    0   -1    1
## [2,]   -1    1    0
## [3,]    1    0    0
## > cache.matrix$getinverse()	
##      [,1] [,2] [,3]
## [1,]    0   -1    1
## [2,]   -1    1    0
## [3,]    1    0    0
## > cacheSolve(cache.matrix)
##      [,1] [,2] [,3]
## [1,]    0   -1    1
## [2,]   -1    1    0
## [3,]    1    0    0
## > example.matrix <- matrix(c(1,0,0,1,1,0,1,1,1), ncol=3, nrow=3)
## > cache.matrix$set(example.matrix)
## > cache.matrix$getinverse()	
## NULL
## > cacheSolve(cache.matrix)
##      [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    0    1   -1
## [3,]    0    0    1
## > cache.matrix$getinverse()	
##      [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    0    1   -1
## [3,]    0    0    1
## > cacheSolve(cache.matrix)
##      [,1] [,2] [,3]
## [1,]    1   -1    0
## [2,]    0    1   -1
## [3,]    0    0    1
## > 

