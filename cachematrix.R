## This file defines two functions:
## 1) create a matrix and 
## 2) cache the inverse of matrix.


## makeCacheMatrix function provides a mechanism to create, retrieve and cache the value of matrix and its inverse. it returns a list of functions which are
## capable to set the matrix and its inverse value in parent enviorment.

makeCacheMatrix <- function(m = matrix()) {
  ## NULL initialization of inverse matrix variable
  inverseMatrix <- NULL
  
  ## Method to set input matrix in parent env.
  setMatrix <- function(matrix) {
    m <<- matrix
    inverseMatrix <<- NULL
  }
  
  ## Method to retrieve matrix
  getMatrix <- function() m
  
  ## Method to set inverse matrix in parent env.
  setInverseMatrix <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  ## Method to retrieve matrix
  getInverseMatrix <- function() inverseMatrix
  
  ## List of methods to be returned
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve function takes above function as argument and always checks the matrix inverse in cache whenever it is called. If inverse matrix found
## in cache then it is delivered from cache else calculated and keep it in cache and deliver to calling environment.

cacheSolve <- function(x, ...) {
  
  ## Return inverse of matrix, if exists 
  inverseMatrix <- x$getInverseMatrix()
  
  ## Whether inverse of matrix already exists
  if(!is.null(inverseMatrix)) {
    ## message notification for availability of inverse of matrix in cache
    message("Found inverse matrix in cache")
    
    ## return the inverse of matrix from cache
    return(inverseMatrix)
  }
  
  ## retrieve the matrix in case inverse not found in cache
  matrix <- x$getMatrix()
  
  ## calculate inverse of matrix
  inverseMatrix <- solve(matrix)
  
  ## verify matrix inversion
  ## print("Multiplication of matrix and its inverse")
  ## print(matrix %*% inverseMatrix);
  
  ## set inverse of matrix to cache
  x$setInverseMatrix(inverseMatrix)
  
  ## return the inverse of matrix
  inverseMatrix
}
