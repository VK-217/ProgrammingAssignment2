## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function takes a matrix as argument and contains methods to get/set matrix and get/set inverse of matrices
## cacheSolve function takes a matrix as argument and checks if inverse exists and returns it if it does.If the inverse does not exist then it gets the inverse of the matrix using solve function and caches it. 
## Write a short comment describing this function
## makeCacheMatrix function takes a matrix as argument and contains methods to get/set matrix and get/set inverse of matrices
makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	set <- function(y) 
	{
		x <<- y
		m <<- NULL
	}
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve function takes a matrix as argument and checks if inverse exists and returns it if it does.
## If the inverse does not exist then it gets the inverse of the matrix using solve function and caches it.
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	m<- x$getinverse()
	if(!is.null(m))
	{
		message("getting cached matrix")
		return (m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
