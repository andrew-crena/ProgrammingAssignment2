## The first function, 'MakeCacheMatrix', creates and object or "matrix", 
## that we will then take the inverse of using the second function, 'cacheSolve', 
## which is a function that takes the inverse of the matrix being interacted with.
## The function will retrieve the the cached inverse if it has already been taken.
## 

## MakeCacheMatrix will create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve will take the matrix you have created and take its inverse,
## unless it has already been cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## test out the functions using:

testmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# creating a testing matrix, applying the cache function
testmatrix$get()

testmatrix$getInverse()

cacheSolve(testmatrix)
#again
cacheSolve(testmatrix)
#output

# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
