## Caching the inverse of a matrix - these functions will partially fullful the 
## Coursera Data Science: R Programming Week 3 Assignment; GitHub user: aorillio 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}


##Testing##

> TestMatrix <- matrix(c(1, 5, 6, 7), 2, 2)
> TestMatrix
     [,1] [,2]
[1,]    1    6
[2,]    5    7

> CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$get()
     [,1] [,2]
[1,]    1    6
[2,]    5    7

> cacheSolve(CacheMatrix)
           [,1]        [,2]
[1,] -0.3043478  0.26086957
[2,]  0.2173913 -0.04347826
