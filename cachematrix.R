## Code is part of John Hopkins Coursera R-Programming Course Assignment 2; Caching the Inverse of a Matrix
## First I have written a function to create and cache a matrix, and then a function to return it's inverse using the
## cached matrix

##  This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
 
		## @x: a square invertible matrix
        	## return: a list containing functions to
        	##              1. set the matrix
        	##              2. get the matrix
        	##              3. set the inverse
        	##              4. get the inverse
        	##     this list is used as the input to cacheSolve()
	
	m<-NULL
  	set<-function(y){
  
		x<<-y
  		m<<-NULL
	}

	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x=matrix(), ...) {
	# @x: output of makeCacheMatrix()
      # return: inverse of the original matrix input to makeCacheMatrix()

	m <- x$getmatrix()
    	
	# if the inverse has already been calculated
	
	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	}
    
	# otherwise, calculates the inverse
	matrix <- x$get()
    	m <- solve(matrix, ...)
    	
	# sets the value of the inverse in the cache via the setinv function.
	x$setmatrix(m)
    	m
} 