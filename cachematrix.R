## These functions will allow a matrix to be stored in the global environment. 
## MakeCacheMatrix is a function that puts the matrix in the global enviroment.  
##  Cachesolve stores the inverse of the matrix in the global environment. 
##  These functions work with the understanding that the matrix is a square matrix. 

makeCacheMatrix <- function(x = matrix()) {
#
        mm <- NULL
        set <- function(y) {
            x <<- y
            mm <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) mm <<- matrix
        getmatrix <- function() mm
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }


## This is a function that 'caches' the inverse of the matrix in the global environment.
## Once the inverse of the matrix is computed it is stored in the global environment.  This operation
## saves computational time when frequently computing the same matrix inversion
## the special programming about this function is the use of the <<- operator. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mm <- x$getmatrix()
        if(!is.null(mm)) {
##            message("getting cached data")
            return(mm)
        }
        data <- x$get()
        mm <- solve(data, ...)
        x$setmatrix(mm)
        mm
    
    
}
