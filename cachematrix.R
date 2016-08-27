## Provide caching of the inverse computiation of a matrix
##
## USAGE:
## > m <- matrix ( runif(100), ncol=10)
## > cm <- makeCacheMatrix(m)
## > inverse <- cacheSolve(cm)
## > assertthat::are_equal(inverse, solve(m))

## Wrap matrix x
makeCacheMatrix <- function (x = matrix()) {
  inversed <- NULL
  set <- function (y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function () x
  setInverse <- function (m) inversed <<- m
  getInverse <- function () inversed
  list (set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  );
}

## Maybe calculate, then return the cached inverse.
cacheSolve <- function (x, ...) {
  inversed <- x$getInverse()
  if (!is.null(inversed)) {
    message("getting cached data")
    return (inversed)
  }
  m <- x$get()
  inversed <- solve(m)
  x$setInverse(inversed)
  inversed
}
