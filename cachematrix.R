## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   
    m_inverse <- NULL  ## holds inverse of matrix
    
    ## Set matrix given by user
    set <- function(y) {
        x <<- y        ## x now holds matrix given by user
        m_inverse <- NULL
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


## Write a short comment describing this function

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
