## caches inverse of a matrix.  Computes inverse if inverse has not been computed


## This function sets a matrix, gets a matrix, sets an inverse, and gets
## an inverse.  Then creates a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set_matrix <- function(y){
   
     x <<- matrix(y, 2, 2)
     i <<- NULL
  }
  
  get_matrix <- function() x
  set_inverse <- function(i) {
    i <<- matrix(y, 2, 2)
  }
  get_inverse <- function() {
    i
  }
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## computes inverse of makeCacheMatrix.  If inverse is already computed, it 
#returns cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$get_inverse()
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get_matrix()
      i <- solve(data, ...)
      x$set_inverse(i)
      i
}
