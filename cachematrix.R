## makeCacheMatrix function creates a special matrix object that can cache
## its inverse. 
## it has 4 function with x(original matrix), i(an inverse of matrix) objects
## To set i(an inverse matrix) object, it has to be passed as an argument
## for cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  # when it is created, or initialize x(matrix) with set function,
  # i is set to be NULL, clearing previous cached data if there's any.
  i <- NULL
  set <- function(y){
    # x and i are stored in their parent environment(makeCacheMatrix instance) 
    x <<- y
    i <<- NULL
  }
  
  # return x(original matrix) value
  get <- function() x
  
  # define setter
  setInverse <- function(inverse) i <<- inverse
  
  # define getter
  getInverse <- function() i
  
  # return a list of named function 
  # so that we can use $ operator to access the functions
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## cacheSolve function computes the inverse of the matrix 
## returned by makeCacheMatrix function.
## it takes makeCacheMatrix() instance as an argument.
## when you call the function with the same instance, return the cached data.

cacheSolve <- function(x, ...) {
  
  # x here is 'makeCacheMatrix' instance
  # initially i object should be NULL
  i <- x$getInverse()
  
  # if above function return cached data, 
  # it just return the value with a message without calculation
  if(!is.null(i)){
    message('getting cached data')
    return(i)
  }
  
  # if i is NULL, retreive a matrix stored in makeCacheMatrix instance,
  # then calculate the inverse of it then store it using setInverse()
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
