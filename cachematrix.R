## This is a pair of functions that cache the inverse of a matrix.
## The <<- operator is used to assign a value to an object in an environment
## that is different from the current environment.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##### EXAMPLE USAGE #####

# > c=rbind(c(4, 3), c(3, 2))
# > c
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > solve(c)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > 
# > m <- makeCacheMatrix(c)
# > m$get()
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > 
