## Functions for creating and using inverted matrices which caching


## Creates cacheable matrix
makeCacheMatrix <- function(original = matrix()) {
  
  inverted <- NULL
  
  set <- function(y) {
    original <<- y
    inverted <<- NULL
  }
  
  # getter and setter for inv. matrix value
  get <- function() original
  # Inversing the matrix using build in solve() function in R
  set_inverted <- function(solve) inverted <<- solve
  get_inverted <- function() inverted
  
  list(
    set = set, 
    get = get,
    set_inverted = set_inverted,
    get_inverted = get_inverted)
}


## Inverses and caches matrix returned by makeCacheMatrix()
## unless inversed matrix is already cached, then uses cached value
## returns inversed matrix
cacheSolve <- function(cacheable, ...) {
  inverted <- cacheable$get_inverted()
  # not cached?
  if(is.null(inverted)) {
    print("cache miss")
    # invert and cache the matrix
    original <- cacheable$get()
    inverted <- solve(original)
    cacheable$set_inverted(inverted)
  } else {
    print("cache hit")
  }
  # return inverted
  inverted
}

test <- function() {
  m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
  cacheSolve(m) # prints 'cache miss'
  cacheSolve(m) # prints 'cache hit'
}

#test()

