## Put comments here that give an overall description of what your
## functions do

## This function helps cache a matrix and its inverse.
## It has 4 parts:
##  - set.  Sets the matrix. Accepts 1 argument, the matrix.
##  - get.  Returns the matrix in the cache.
##  - setinverse.  Sets the inverse of the matrix. Accepts 1 argument, the
##                 inverse of the matrix.
##  - getinverse.  Returns the inverse of the matrix in the cache.
## 
##  Arguments:
##  - matrix


makeCacheMatrix <- function(x = matrix()) {
   
    m_inverse <- NULL  ## holds inverse of matrix
    
    ## Set matrix given by user
    set <- function(y) {
        x <<- y        ## x now holds matrix given by user
        m_inverse <<- NULL
    }
    
    ## return matrix to user
    get <- function() x
    
    ## set the inverse of a matrix
    setinverse <- function( inverse ) m_inverse <<- inverse
    
    ## get the inverse of a matrix
    getinverse <- function() m_inverse
    
    list (set = set, 
          get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## This function calculates the inverse of a matrix and caches it inside 
## the object passed into it.
## It has at least 1 argument, which is the object (makeCacheMatrix).
## It returns the inverse of the matrix, either from cache or from a 
## calculation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m_inverse <- x$getinverse()      ## m now holds the inverse in object x.
    if (! is.null(m_inverse) ) {
        message("getting cache data")
        return (m_inverse)
    }
    
    ## inverse is not yet available in x.  Calculate, set in obj x, and return
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}
