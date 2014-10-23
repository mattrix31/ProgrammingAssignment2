## makeCacheMatrix creates, changes, or solves a solveable matrix.  CacheSolve either retrieves cached data
## from matrix, or calcuates then caches solve function output

## 4 subfunctions available to makeCacheMatrix defined here

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## temp holding place for inverse - NULL means it has not been calculated
  set <- function(y) { #if matrix is changed, clear cached data m
    m <<- NULL
    x <<- y
  }
  get <- function() x
  setinverse <- function(solve)  m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculates inverse of matrix, stores it using setinverse function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { #get and return cached data if it exists
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setinverse(m)
  m
}
