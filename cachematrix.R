## Put comments here that give an overall description of what your
## functions do

## This functio wrapes the matrix to includes stored data of inver matrix (cache)

makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(newmx) {
        x <<- newmx
        x.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x.inv <<- inverse
    getinverse <- function() x.inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix stored (cached) in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
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
