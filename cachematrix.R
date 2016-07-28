## Diego Alducin
## Coursera : R Programming
## Assignment 2

## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set value of the matrix
  m_inv <- NULL
  set <- function(y){
    x <<- y ## '<<-' used to assign a value to an object in an enviroment
    m_inv <<- NULL 
  }
  
  ## Get the value of the matrix
  get = function() x
  
  ## Set the inverse
  set_inv = function(inverse) m_inv <<- inverse
  
  ## Get the inverse
  get_inv = function() m_inv
  
  ##list arguments
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}

## cacheSolve: This function computes the invers of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesholve should retrieve the inverse of the cahse.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##get the inverse matrix
  m_inv = x$get_inv()
  
  ## if logic to return null if inverse has been calculated
  if(!is.null(m_inv)){
    ## message if inverse already calculated and return cached data
    message("matrix is inverse, getting cached data")
    return(m_inv)
    
  }
  
  ## if not, caculate the inverse
  data = x$get()
  m_inv = solve(data,...)
  
  ## set inverse
  x$set_inv(m_inv)
  
  ## return inverse matrix
  m_inv
  
  
}
