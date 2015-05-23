## creates a matrix object that can be modified using setmatrix. 
##Then checks the cache to see if the inverse has been computed and stored. 
##If it has not been cached, then it is computed using solve().  

## makeCacheMatrix sets and gets the value of a matrix, and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve checks to see if inverse has been cached, 
##returns inverse if it has, computes inverse if it has not. 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)){
        message("geting cached data")
        return(m)    
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
