#set the value of the matrix
#get the value of the matrix
#set the value of the inverse 
#get value of the inverse

#This function will create a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m 
	list(set = set, 
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
	
}


##This function will check whether the inverse has been caculated. If so, it would skip the computation and get the calculated value from the cache. If not, it will calculate the inverse and set the inverse of the function via setinverse.

cacheSolve <- function(x, ...) {
	     m <- x$getinverse()
	     if(!is.null(m)){
	     	message('getting cached data')
	     	return(m)
	     }
	     data <- x$get()
	     m <- Solve(data,...)
	     x$setinverse(m)
	     m
        ## Return a matrix that is the inverse of 'x'
}
