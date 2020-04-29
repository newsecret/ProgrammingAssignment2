
makeCacheMatrix <- function(x = matrix()) { ## Creating matrix
  m <- NULL ## Holds value of inverse matrix
  set <- function(y) {
    x <<- y ##parent environment
    m <<- NULL ##set to null if new matrix
  }
  get <- function() x ##returning value of the matrix argument
  setinverse <- function(Inverse) m <<- Inverse ##Asigning the value
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##refer to the function
}

cacheSolve <- function(x, ...) { ##Catche function
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
