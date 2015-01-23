#name       : makeCacheMatrix 
#description: Get a vector of the matrix
#i/p param  : matrix x
#o/p param  : numeric vector

makeCacheMatrixVector <- function(x = matrix()) {

	i <- NULL                                   #initially setting the inverse of the matrix to NULL
	set <- function(y) {
		x <<- y                             #setting the value of inverse of the matrix from cache
		i <<- NULL
	}
	get <- function() x                         #getting the value of the matrix x
	setinverse <- function(solve) i <<- solve   #setting the value of the inverse in the cache
	getinverse <- function() i                  #getting the value of the inverse that was set earlier
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)               #list of functions in makeCacheMatrix

}


#name       : cacheSolve 
#description: Get the inverse of matrix x using getinverse and setinverse functions
#i/p param  : matrix x
#o/p param  : inverse of the matrix

cacheSolve <- function(x, ...) {
	
	i <- NULL

	i <- x$getinverse                          #getting the inverse of the matrix 
	if(!is.null(i)){                           #If i is not NULL, we have retrieved cached value for the inverse
		message("getting cached inverse")
		return(i)                            
	}
	data <- x$get()                            #getting the matrix value of x as inverse is not present in cache
	i<- solve(data,...)                        #getting inverse value for the matrx using solve() function
	x$setinverse(i)                            #setting the value of the inverse in cache

	i                                          #returning the value of the inverse of the matrix
}
