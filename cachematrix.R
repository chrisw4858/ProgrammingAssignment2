# These functions provide an optimized technique of calculating the inverse of a matrix.
# Once calculated, the inverse is cached and further invocations return the cached value
# unless the value of the matrix is changed.  
#
# Usage is of the form:
#
#    m <- matrix(    )
#    mcache <- makeCacheMatrix(m)
#    inverse <- cacheSolve(mcache)
#
# The value of the cached matrix can be changed:
#
#    m <- matrix(    )
#    mcache$set(m)
#    cacheSolve(mcache)
#
# The matrix must be a square invertible matrix




# Create a 'special' matrix that provides a mechanism to store the original matrix
# and store the inverse of the matrix. Functions to get & set the matrix, and
# get & set the inverse are provided.
makeCacheMatrix <- function(m = matrix()) {
    
    # Note: the inverse is not calculated until needed - indicated by NULL
    inverse <- NULL
    
    # Set (store) a matrix.  Note: if changing the matrix, must reset the matrix, 
    # and reset the inverse back to NULL
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    
    # Return the original matrix
    get <- function() m
    
    # Set (store) the inverse of the matrix
    setinverse <- function(i) inverse <<- i
    
    # Retreive the inverset of the matrix
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function will return the inverse of the supplied matrix in an optimized manner.
# On the first invocation of this function for a given matrix the inverse is calculated 
# using solve() and cached.  On subsequent invocations the cached inverse will be returned,
# unless the matrix has changed.
#
# Assumptions:
# - the matrix x is a special form created using makeCacheMatrix()
# - the provided matrix is a square invertible matrix

cacheSolve <- function(x, ...) {
    # Return the cached inverse, if it has been previously calculated
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Inverse has not been cached previously, so calculate inverse using solve()
    # and then cache the inverse for future use
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
