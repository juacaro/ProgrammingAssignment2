## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invm <<- solve
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    if(det(data) == 0){
        return(message("Indeterminate matrix - Determinant equal to 0"))
    }
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
