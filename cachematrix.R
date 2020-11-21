## Put comments here that give an overall description of what your
## functions do
## I will write 2 functions, namely, "makeCacheMatrix" and "cacheSolve" to cache 
## the inverse of a matrix.



## Write a short comment describing this function

## “makeCacheMatrix” is a function which creates a special "matrix" object that 
## can cache its inverse for the input (which is an invertible square matrix).
## So assuming the matrix supplied is invertible, NULL (a reserved word in R 
## representing null objects) is assigned to a variable called "inv".
## Then the value of matrix is set using "set" function and stored in variable called "set".
## In "set" function, double arrow assignment operator is used modify variables in parent levels.
## Unlike the usual single arrow assignment (<-) that always works on the current
## level, the double arrow operator (<<-) can modify variables in parent levels.
## Note: assign(x, value, inherits=TRUE) can also be used instead of double arrow operator.
## <<- is most useful in conjunction with closures to maintain state.
## A closure is a function written by another function. 
## Closures are so called because they enclose the environment of the parent 
## function, and can access all variables and parameters in that function. 
## This is useful because it allows us to have two levels of parameters. 
## One level of parameters (the parent) controls how the function works. 
## The other level (the child) does the work.
## In our case, makeCacheMatrix is the parent function, function(y) is the child function.
## Thus, the key to managing variables at different levels is the double arrow assignment operator <<-.
## The ability to manage variables at two levels also makes it possible to maintain 
## the state across function invocations by allowing a function to modify variables in the environment of its parent.
## Outside the "set" function, we willl get the value of the matrix using "get" function.
## Then we will get the value of inverse using "setInverse". Here the double arrow operator is used again.
## Then we get the value of the inverse using "getInverse".
## Then we created a list



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set  <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() {x}
  setInverse <- function(inverse) {inv <<-inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## “cacheSolve” is a function which computes the inverse of the special "matrix"
## returned by “makeCacheMatrix” above. If the inverse has already been 
## calculated and the matrix has not changed, the “cachesolve” should retrieve
## the inverse from the cache.
## First line of code returns the matrix of 'x', and assign it to inv
## Next, we need to check whether or not inverse has been calculated. If inverse has already been calculated, it can be retrieved from cache and computation can be skipped.
## If inverse is retrieved from cache, the message "getting cached data" will be displayed and the inverse will be returned.
## Else, the inverse of the matrix will be calculated and set to cache.
## Inverse is computed using solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
