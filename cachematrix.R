## These functions will allow a matrix to be stored in the global environment. 
## MakeCacheMatrix is a function that puts the matrix in the global enviroment.  
##  Cachesolve stores the inverse of the matrix in the global environment. 
##  These functions work with the understanding that the matrix is a square matrix. 

makeCacheMatrix <- function(x = matrix()) {
##This function contains four functions. The functions are set, get, setmatrix, getmatrix
##  the Set function sets x to the matrix input to the makeCacheMatrix.  x is saved in the Gloabl Environment     
        mm <- NULL
        set <- function(y) {
            x <<- y
            mm <<- NULL
        }
        
        get <- function() x     # get returns the matrix from the global environment.        
        
        ## setmatrix stores the matrix in the global environment
        setmatrix <- function(matrix) mm <<- matrix
        
        getmatrix <- function() mm    ## getmatrix retrieves the matrix from the global environment
        list(set = set, get = get,    ## return a list of functions. 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }


## cacheSolve will invert the matrix is the matrix has not previously been inverted
## the inverted matrix will be stored, 'cached' in the global environment and returned to the user.
## 
## Once the inverse of the matrix is computed it is stored in the global environment.  This operation
## saves computational time when frequently computing the same matrix inversion
## the special programming about this function is the use of the <<- operator. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mm <- x$getmatrix()
        if(!is.null(mm)) {    ##  check to see if the inverted matrix has been stored in the global environment
            message("getting cached data")  ## this is a debug/status statement to ensure cached data is returned 
            return(mm)                      ## exit the function here
        }
        data <- x$get()       ## if the inverse of the matrix had not previously been computed, compute it now
        mm <- solve(data, ...)     ## the matrix is inverted with this command 
        x$setmatrix(mm)            ## the inverted matrix is stored with this command 
        mm
    
    
}
