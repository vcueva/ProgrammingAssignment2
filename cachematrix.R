## The first function creates a matrix object and return the functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse matrix
## 4. get the value of its inverse matrix
## The second calculates the inverse of a matrix created with the
## makeCacheMatrix function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data and
## sets the value of the inverse in the cache via the setinverse function.


## makeCacheMatrix creates a matrix object and returns the functions
## to manage it.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	# set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	# get the value of the matrix
        get <- function() x
	# set the value of its inverse matrix
        setinverse <- function(inverse) i <<- inverse
	# get the value of its inverse matrix
        getinverse <- function() i
	# returns the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix x created with the last
## makeCacheMatrix and returns the result using cache.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
	# obtain the inverse from the cache
        i <- x$getinverse()
	# if it is not null then return this value.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	# else calculate the inverse
        data <- x$get()
        i <- solve(data)
	# set the value to the cache
        x$setinverse(i)
	# return the calculated inverse
        i
}
