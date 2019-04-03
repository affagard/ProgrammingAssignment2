## These functions calculate the inverse of an invertible matrix with cache management
## If the inverse matrix has already been computed, the code doesn't resolve it again and return the cached result
## If the inverse matrix is not in cache, then the code solve it , cache it and return this new matrix.
##
## Note that this code doesn't deal with non invertible matrix 
##
## Example of using : 
##      x <- matrix (c(-1,1,2,3), nrow = 2, ncol = 2)
##      z <- matrix (c(1,1,1,5), nrow = 2, ncol = 2)
##      y <- (makeCacheMatrix(x))
##      cacheSolve(y)               # First launch, return the calculated inverse of x
##      cacheSolve(y)               # Second launch, return the cached inverse of x
##      y$set(z)                    # Remplace content of y with another matrix z


## Constructor for the object to manage the caching
##
##      set : method to input a new matrix and clean the cache
##      get : method to retrieve the content of the current matrix
##      setsolve : method to put in cache the just calculated inverse matrix
##      getsolve : method to retrieve the matrix currently in cache
##
##      input x : an invertible matrix
##      return : an instance of the object makeCacheMatrix
##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
       x <<- y
       s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function to return the inverse of a matrix with a cache management of the result
##
##      input x : an object constructed with the function above makeCacheMatrix()
##      return s : the inverse of the matrix, calculated or from cache
##
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
