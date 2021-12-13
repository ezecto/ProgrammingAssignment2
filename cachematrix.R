makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function() inv <<- solve(a)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(a, ...) {
        m<-a$getinverse
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-a$get
        m <- inv(data, ...)
        x$setinverse(m)
        m
}
