## Caching the Inverse of a Matrix

## This function creates a special "matrix", which is really a list containing a function to
##      a. set the value of the matrix
##      b. get the value of the matrix
##      c. set the value of the inverted matrix
##      d. get the value of the inverted matrix


makeCacheMatrix <- function(mat = matrix()) {
        inver <- NULL
        set <- function(x) {
                mat <<- x
                inver <<- NULL
        }
        get <- function() mat
        setmat <- function(invert) inver <<- invert
        getmat <- function() inver
        list(set=set, get=get, setmat=setmat, getmat=getmat)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(mat, ...) {
        inver <- mat$getmat()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        dat <- mat$get()
        inver <- solve(dat)
        mat$setmat(inver)
        inver ## Return a matrix that is the inverse of 'x'
}
