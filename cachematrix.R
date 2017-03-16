## This function, makeCachematrix creates a special matrix, which does the following
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the the matrix inverse
## 4. Get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse)m <<- solve
	getinverse <- function()m
	list(set = set, get = get, 
	setinverse = setinverse,
	getinverse = getinverse)
}


## This function, cacheSolve solves for the inverse of the special "matrix" created
## with the function above. It first checks to see if the inverse has already been 
## solved. If so, it gets the inverse matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (is!null(m)) {
        	message ("getting chached data")
        	return(m)        	
  		}
  		data <- x$get()
  		m <- solve(data, ...)
  		x$setinverse(m)
  		m
}
