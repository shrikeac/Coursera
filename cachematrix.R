# assumed that the input is a square matrix that has an inverse
# otherwise error will be thrown: Error in solve.default(data) : 'a' (m x n) must be square  
makeCacheMatrix <- function(x= matrix()) {
	# initializes the inv property when makeCacheMatrix is 1st initialized
	inv <- NULL
	# the setter method to override the input matix passed in during initialization
    set <- function(y) {
		# <<- means to modify a variable in the parent environment
        x <<- y
        inv <<- NULL
    }
	# getter method
    get <- function() x
	# sets the inverse in the parent environment
    setInv <- function(inverse) inv <<- inverse
	
	# getter for inverse
    getInv <- function() inv
	
	# returns list of functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

# input is list of function returned by makeCacheMatrix
cacheSolve <- function(x) {
	inv <- x$getInv()
	# here is the cache check for the inverse... 
	# if inv exists then just return
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	# otherwise do the hard work of calling solve
	data <- x$get()
	inv <- solve(data)
	# and then setting inv in cache
	x$setInv(inv)
	# return inv
	inv
}