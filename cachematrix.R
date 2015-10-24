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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mm <- x$getmatrix()
        if(!is.null(mm)) {
            message("getting cached data")
            return(mm)
        }
        data <- x$get()
        mm <- solve(data, ...)
        x$setmatrix(mm)
        mm
    
    
}
