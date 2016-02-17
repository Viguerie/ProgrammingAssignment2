## These two function provide a means of caching the inverse of a matix 
## using the "<<-" assignment operator

## This function takes a matrix as its input and returns a list of four
## functions. The four functions perform the following operations:
##		1. set the value of the matrix (set())
##		2. get the value of the matrix (get())
##		3. set the value of the inverse of the matrix (setinv())
##		4. get the value of the inverse of the matrix (getinv())

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
         x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(inverse) {inv <<- inverse} 
    getinv <- function() {inv}
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function return a matrix that is the inverse of 'x'.
## The function first checks to see if the inverse has already been calculated. 
## If so, it prints the message "getting cached data" gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## matrix using the solve(x) function and sets the value of the inverse in the 
## cache via the setinv() function.

cacheSolve <- function(x, ...) {
        
	inv = x$getinv()
        
    if (!is.null(inv)){
		message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
