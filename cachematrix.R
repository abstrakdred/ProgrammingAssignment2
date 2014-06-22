## Put comments here that give an overall description of what your
## functions do
#
# This script is an example of lexical scoping which makes use of the <<- assignment operator which allows a function to access
# variables that reside in that functions' parent environment.
#
# In the example x and iMatrix are defined in, and appear to be local to makeCacheMatrix(),
# but cacheSolve()

## Write a short comment describing this function
#
# makeCacheMatrix() accepts a square matrix as input and returns a list of
# functions that can access or operate on that matrix.
# The function defines the variable iMatrix as a cache that will store the 
# inverse of the matrix
#
makeCacheMatrix <- 
function(x = matrix()) 
{
	# Initialise the the empty cache
	iMatrix <- NULL
	
	# Use the set() function to initialise the new matrix value, while initialisng the cache to an empty state
	set <- function(y)
	{
		x <<- y
		iMatrix <<- NULL
	}
	
	# return the current matrix
	get <- function() x
	
	# set the inverse matrix cache to a valid inverse matrix
	setIMatrix <- function(i) iMatrix <<- i
	
	# return the current value of the cache
	getIMatrix <- function() iMatrix
	
	#return the list object with function items that give access to the matrix and inverse matrix cache
	list(set = set, get = get, 
	      setIMatrix = setIMatrix, getIMatrix = getIMatrix)
}


## Write a short comment describing this function
#
# cacheSolve accepts a list object created by the makeCacheMatrix() function
# and creates, then saves it to the 'cache' variable, iMatrix', and then returns an 
# inverse matrix if it has not already been defined, ie, the cacheSolve hasn't already 
# been called on the list object.
#
# Otherwise this function will just return the inverse matrix previously cached.
#
#

cacheSolve <- 
function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    # attempt to get the inverse matrix
	im <- x$getIMatrix()
	
	# Verify whether an inverse matrix has been cached
	if( !is.null( im))
	{   # An inverse matrix has already been saved, lets just return it here
		message("getting cached data")
         return(im)
	}
	
	# A new inverse matrix is about to be calculated, and saved to its cache, and the matrix returned back
	data <- x$get()
	im <- solve(data)
	x$setIMatrix(im)
	im
}
