## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   # Create a special "matrix" object  
    inv <- NULL   # Initialize the cached inverse as NULL

    set <- function(y) {   # Function to set a new matrix
        x <<- y
        inv <<- NULL   # Reset cached inverse to NULL
    }
   
    get <- function() x
   
    setInverse <- function(inverse) inv <<- inverse
   
    getInverse <- function() inv
    
    list(set = set, get = get,   # Return a list of functions
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse() 
    if(!is.null(inv)) {     # If cached, return it
        message("getting cached data")  
        return(inv)     # Return cached inverse
    }

    mat <- x$get()    # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)       
    inv           # Return the inverse             
}


