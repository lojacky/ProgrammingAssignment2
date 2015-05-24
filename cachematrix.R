## Put comments here that give an overall description of what your
## functions do


#This function is to store the cache matrix and the related invertible matrix, 
#You can retrieve the data by using $getMatrix, $getInvertibleMatrix and set the data by using $setMatrix(matrix), and $setInvertibleMatrix(matrix)

# Assumption: The matrix is always invertible.
#Input: Matrix
#Output: The address of difference functions.

makeCacheMatrix <- function(matrix_data = matrix()) {
  invertibleMatix <- NULL
  setMatrix <- function(y) {
    matrix_data <<- y
    invertibleMatix <<- NULL
  }
  getMatrix <- function() matrix_data
  setInvertibleMatrix <- function(solve) invertibleMatix <<- solve
  getInvertibleMatrix <- function() invertibleMatix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvertibleMatrix = setInvertibleMatrix,
       getInvertibleMatrix = getInvertibleMatrix)
}



# Assumption: The matrix is always invertible.
#This function retrieve the invertible matrix if we have stored it. Otherwise, it will calculated the invertible matrix by using solve function.
#Input: makeCacheMatrix function
#Output: Invertible Matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvertibleMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInvertibleMatrix(m)
  m
}
