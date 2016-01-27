## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This creates a function to set/get and set/get inverse of a matrix cache
makeCacheMatrix <- function(x = matrix()) {
  mt <- NULL; #Temp Variable
  set <- function(my) {
    x <<- my; # Set the Input to matrix my that is called in set
    mt <<- NULL; #Empty Temp
  } 
  get <- function() x; # Return the Cache
  setinverse <- function(solve) mt <<- solve; # Set the inverse as the input
  getinverse <- function() mt; # Return the cached inverse 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse); #Function calls list
}


## Write a short comment describing this function

cacheSolve <- function(x= matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Call the inverse to see if it already exists
  omt <- x$getinverse(); 
  if(!is.null(omt)) { #If exists, return cahced value
    message("Getting Cached inverse");
    return(omt);
  }
  #We don't have a cache
  data <- x$get(); #Get the inputs
  #RD Added - If Determinant of yoru matrix is 0, it can't be inverted
  if (det(data) == 0) {
    message("Determinant of matrix is 0, hence matrix is not invertible");
    return (NULL);
  }
  omt <- solve(data,...); #Compute the inverse
  x$setinverse(omt); #Store the inverse
  omt; #Return the inverse as the last call
}

#Test
a <- makeCacheMatrix(); #set the matrix
a$set(matrix(1:4,2,2)); #make sure it is a square
a$get(); #print original matrix
cacheSolve(a); #Now solve it ; 1st time
cacheSolve(a); #Now solve it ; 2nd time


b <- makeCacheMatrix(); #set the matrix
b$set(matrix(rnorm(9),3,3)); #make sure it is a square
b$get(); #print original matrix
cacheSolve(b); #Now solve it ; 1st time
cacheSolve(a); #Now solve a ; 3rd time
cacheSolve(a); #Now solve b ; 2nd time - from cache

#Test Trap non-invertible
c <- makeCacheMatrix(); #set the matrix
c$set(matrix(matrix(1:16),4,4)); #make sure it is a square
c$get(); #print original matrix
cacheSolve(c); #Now solve it ; 1st time
