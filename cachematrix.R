## Matrix Inversion is lengthy computation and there is merit in caching the 
## inverse of the output. The following two programs achieve this objective
## Also program uses the model of the example provided in the assignment


## This function creates a list by setting and getting the values 
## of matrix and its inverse as well

makeCacheMatrix <- function (x = matrix()) {
      i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function returns the inverse of a matrix. It checks if inverse is already
## computed. If null then gets the inverse from cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
