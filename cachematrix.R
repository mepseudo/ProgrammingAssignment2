## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    This function returns a LIST of 4 functions as getter/setter of the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  if ( ncol(x)!= nrow(x))    print("Error:  matrix is not square");
  cached_inv <- NULL
  set <- function(y) {
    x <<- y
    cached_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) cached_inv <<- inv
  getinverse <- function() cached_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##    NOTE: If input not a SQUARE MATRIX then error

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse();
  if(!is.null(inverse)) {
    message("getting cached data");
    return(inverse);
  }
  data <- x$get()
  if ( ncol(data) != nrow(data)) {
    print("Error:  matrix is not square");
    return(NULL);
  } else {
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse  
  }
}  

## TEST CASES  ## TEST CASES  ## TEST CASES
#Take a simple 2x2 matrix
myMatrix<-matrix(1:4,2,2)
#Create a special matrix with a cache
myCacheMatrix<-makeCacheMatrix(myMatrix)
#Access the special matrix for the first time, filling the cache
cacheSolve(myCacheMatrix)
#Access the special matrix for the 2nd time, notice that it uses the cache.
cacheSolve(myCacheMatrix)

#Now take a 2 by 3 matrix
myMatrix<-matrix(1:6,2,3)
# create special matrix, but get ERROR since not invertable
myCacheMatrix<-makeCacheMatrix(myMatrix)
# Try to access matrix, get error, not invertible
cacheSolve(myCacheMatrix)


