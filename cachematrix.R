## Set functions that cache the inverse of a matrix

# create a object with matrix functionsWrite a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initiate inverse of matrix
  inv <- NULL
  # set and get the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   
  # set and get the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # return the matrix functions list
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)

}


## caculate the inverse of matrix, if inverse matrix has already 
# been caculated, cacheSolve can return the inverse from the cache

cacheSolve <- function(x, ...) {
  # return a inverse matrix of x
  inv <- x$getinverse()
  # caculate if the inverse matrix exists
  if(!is.null(inv)) {
    message("getting cached data.")
    # return cached inverse matrix
    return(inv)
  }
  # Get the object matrix
  data <- x$get()
  # use solve to caculate the inverse matrix
  inv <- solve(data,...)
  # set the inverse to object
  x$setinverse(inv)
  # return inverse matrix
  inv
}
