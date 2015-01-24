## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This is a 'function' which seems to be basically
# emulating a class construct ie we have set & get functions
# & we have scope rules which keep our variables from
# being accessed without the set & get functions
makeCacheMatrix <- function(x = matrix()) {
            ix <- NULL
            set <- function(y) {
                    x <<- y
                    ix <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) ix <<- inverse
            getinverse <- function() ix
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function
# this function will only find the inverse of a
# matrix if it has not already found it
# This function expects as input the output of
# makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("getting cached inverse")
        return(ix)
    }
    m <- x$get()
    ix <- solve(m)
    x$setinverse(ix)
    ix
}
