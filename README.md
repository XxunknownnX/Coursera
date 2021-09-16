# Coursera
make_CacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setting_Inverse <- function(inverse) {
    i <<- inverse
  }
  getting_Inverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) %*% data
  
  x$setInverse(m)
  m
}
