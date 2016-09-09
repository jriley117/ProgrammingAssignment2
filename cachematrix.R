## this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) { ## Set value of matrix
        x <<- y
        invert <<- NULL
    }
    get <- function() x ## Get value of matrix
    setinverse <- function(inverse) invert <<- inverse ##Set value of the inverse
    getinverse <- function() invert   ##Get value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function returns the inverse of the special "matrix" returned by makeCacheMatrix
## It checks if the inverse has already been calculated. If not, it computes the inverse & sets the value in the cache. 

cacheSolve <- function(x, ...) {
    invert <- x$getinverse()
    if(!is.null(invert)) {   ##if inverse already computed, it retrieves from the cache
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...) ## computes the inverse
    x$setinverse(invert)  ## sets value in the cache
    invert
}