
## The functions below provide the capability to cache potentially 
## time-consuming computations for matrix inversion.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## Initialize m to null
        set <- function(y) {                    ## Changes the matrix x stored in the main function
                x <<- y                         ## Substitute the matrix x with the input y in the 
                m <<- NULL                      ## main function.  
        }
        get <- function() x                     ## Returns the matrix x stored in main function
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,              ## Stores 4 functions
                setinv = setinv,
                getinv = getinv)
        }


##The cachesolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()                         ## Verify the value of m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                   ## Compute the inverse of matrix
        x$setinv(m)
        m                                       ## Return a matrix that is the inverse of 'x'
        }        
        