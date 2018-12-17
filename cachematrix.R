##makeCacheMatrix function definies an empty matrix and its inverse and 
##defines the setter and getter functions for both the matrices

##cacheSolve method accepts a matrix as a parameter
## checks if the matrix already contains a cache value of its inverse
## if the cache inverse matrix value exists, returns the cache value
## if the cache inverse matrix value does not exists, calculates the 
## inverse, sets to cache and returns the calculated matrix


## makeCacheMatrix function defines an empty matrix and its inverse
## makeCacheMatrix also defines setter and getter methods for both the matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## set the matrix value to null
  m <- NULL
  ## setter method for the original matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## getter method for original matrix
  get <- function() x
  
  ## setter method for Inverse matrix
  setInverse <- function(inverse) m <<- inverse
  
  ## getter method for Inverse matrix
  getInverse <- function() m
  
  ## list to maintain all the setter and getter methods for both original and inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve returns the cached Inverse of the given matrix if exists. 
## if the cachedValue does not exist, it calculates the inverse of the matrix, stores it in cache
## and then returns the calculated inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() ##get the inverse of the matrix by calling the getter method of inverse matrix
        
    ##check if the value of the inverse matrix is not null. If it is not null then get the cached inverse matrix data    
    if(!is.null(m)) {
            ## display a message to the end user indicating that the cached value is being returned.
            message("getting cached data")
            ## return the cached inverse matrix
            return(m)
    }
    ##get the original matrix by using the matrix get method that was defined in makeCacheMatrix
    data <- x$get()
    ## calculate the inverse of the matrix by calling the solve method on the original matrix
    m <- solve(data)
    ## set the calculated inverse matrix into the cache by calling the setter method of inverse matrix 
    ## defined in makeCacheMatrix
    x$setInverse(m)
    ## return the calculated inverse matrix
    m
}
