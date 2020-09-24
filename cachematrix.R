# A function that creates a list object of getters and setters for the ORIGINAL matrix and the INVERSED ORIGINAL matrix.
makeCacheMatrix <- function(originalMatrix = matrix()) {
  inversedMatrix <- NULL
  
  set <- function(setFunctionMatrix) {
    originalMatrix <<- setFunctionMatrix
    inversedMatrix <<- NULL
  }
  
  get <- function() originalMatrix
  
  setInverse <- function(inversed) inversedMatrix <<- inversed
  
  getInverse <- function() inversedMatrix
  
  list( set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
      )
}

# A function object that gets the inverse of the list object created by the makeCacheMatrix() function.
# If the inverse of the object exists it returns the existing inverse matrix
# else it calculates the inverse of the object and caches it and then returns the inverse matrix.
cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInverse()
  
  if (!is.null(inversedMatrix)) {
    print("getting cached inverse!")
    return (inversedMatrix)
  }
  
  data <- x$get()
  
  inversedMatrix <- solve(data, ...)
  
  x$setInverse(inversedMatrix)
  
  inversedMatrix
}
