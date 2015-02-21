## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix makes a list of 4 functions.
# 1st function is to set 2 variables(x and m) in parent's environment(makeCacheMatrix) as parameter y and NULL.
# 2nd function is to get the value of x
# 3rd function is to set m as a function solve
# 4th function is to get the value of solve
# finally, return the list of set, get, setsolve and getsolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve is a function with parameter whose object is the result of makeCacheMatrix.
# cacheSolve returns inverse of a matrix by computing if m is null else by getting the value in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
