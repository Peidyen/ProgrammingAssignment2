##


## Creates a makeCacheMatrix function which allows one to get, set, the matrix as well as get and set the ## inverse of the matrix via the solve function.  Allows for the caching of the matrix


makeCacheMatrix <- function(x = matrix()){    
  m <- NULL
  set <- function(y){
    x <<- y  
#store matrix in cache  
 m <<- NULL 
  }
  
  #get matrix
  get <- function() x 
  
  #set inverse matrix
  setInverse <- function(solve) m<<- solve 
  
  #get inverse matrix
  getInverse <- function() m 
  
  ## create list of functions, similar to the revealing module pattern in javascript
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache, otherwise it solves for the inverse.

cacheSolve <- function(x, ...) {
      
      ## Create matrix, inverse of x
      
      m <- x$getInverse()                 
	  #request cache
	  #is there a cached value?
      if(!is.null(m)){                    
          message("getting cached data")  
          return(m)                       # return cached value
        }
		
		# get matrix 
      data <- x$get()                     
	  # solve the inverse
      m <- solve(data, ...)               
	  # set the inverse using the make cache function
      x$setInverse(m)                     
    
	#evaluate m to return it to the variable data
	  m
	  }
