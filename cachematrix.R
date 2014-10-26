## This is a two function solution to cache inverser of an invertible matrix. 
## Matrix must be created using the makeCacheMatrix function to be able to retrieve the inverse from the cache instead of 
## re-computing the inverse of a mtrix for already inverse computed matrices.

## makeCacheMatrix function creates the matrix along with a list of functions for the matrix
## makeCacheMatrix provides functions to get the matrix as well as set the inverse matrix for it.
## Though a set funtion is provided, that should not be called as that would change the matrix to its inverse mapping wrong.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function retruns the inverse of the passed matrix x.
## cacheSolve first tries to get the invesre of a matrix from cache, if it does not exist then inverse is computed and stored, 
## so that next call for invesrse of the same function can be retrieved from cache instead of re-computing
## since this special matrix is passed to the cacheSolve function, makeCacheMatrix environment becomes the 
## operating environment for cachSolve function and all the functions of makeCacheMatrix can be invoked from cachSolve function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        print(m)
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
        m
}
