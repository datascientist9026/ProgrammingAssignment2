## makeCacheMatrix takes matrix as input and returns 
## four functions and two variables. "set" function sets the
# matrix. "get" function gets the matrix for us. "setinverse" function
## sets the inverse of the given matrix in variable m. And "getinverse"
## gets the calculated inverse for us. 



makeCacheMatrix <- function(x = matrix()) {
			
			m <- NULL
      
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      
      get <- function() x
      
      setinverse <- function(inversemat) m <<- inversemat
      
      getinverse <- function()  m
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve takes matrix as input and it checks if the inverse
## of the given matrix is already calculated. If its not calculated, then
## it does calculate it and returns it. If the inverse is already calculated, then
## it displays the message "getting cached data" and returns already calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
            
            if(!is.null(m)) {
              message("getting cached data")
              return(m)
            }
            
            data <- x$get()
            
            m <- solve(data,...)
            
            x$setinverse(m)
            m
}
