## Author:Ignacio Ferrero
## Program: cachematrix.R
## Descirption: This programm caches the inverse of a matrix, to make it more 
##              efficient and less costly for future use

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  inv_matrix <- NULL
  set <- function(y) 
  {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inv_matrix <- x$getinverse()
  #look if cached
  if(!is.null(inv_matrix)) {
    message("getting cached data.")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinverse(inv_matrix)
  inv_matrix     
}

##TESTING
##RESULTS
##matrix <- rbind(c(2, 3), c(4, 3))
## value = makeCacheMatrix(matrix)
##value$get()
##[,1] [,2]
##[1,]    2    3
##[2,]    4    3
## cacheSolve(value)
##[,1]       [,2]
##[1,] -0.5000000  0.5000000
##[2,]  0.6666667 -0.3333333