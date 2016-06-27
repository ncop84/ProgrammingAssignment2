## makeCacheMatrix takes an invertable matrix as argument and creates a list of 
## of functions that can return and cache the matrix itself, and also return and 
## cache the inverse of such matrix.
##
## cacheSolve takes the object created by makeCacheMatrix as an argument and
## calculates the inverse of the matrix if no inverse has been calculated
## before. If an inverse was cached already, it just returns the inverse of the
## matrix from the cache.

## This function creates the special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        # set the inv.m (inverse matrix) variable to NULL, as it has not been
        # determined at the moment of making the matrix object
        inv.m <- NULL
        
        setM <- function(y) {
                
                x <<- y 
                # Substitue x with y in the main function, and not only in the 
                # WriteM function, such that x is 'cached'
                
                inv.m <<- NULL
                # This provides a null value to the inversed matrix, as a 
                # potentially 'old' inverted matrix is no longer relevant to 
                # the newly set x.
        }
        
        # Returns matrix x as stored in the main function
        getM <- function() x 
        
        
        setInvM <- function(y) {
                
                # Store the inverse matrix in the main function
                inv.m <<- y
                
        } 
        
        # Returns the inverse of matrix x as stored in the main function
        getInvM <- function() inv.m
        
        
        # Outcome of the makeCacheMatrix function is a list containing the
        # Set/Get M/InvM functions as generated above.
        list(setM = setM, getM = getM,
             setInvM = setInvM,
             getInvM = getInvM)
        
}


## cacheSolve solves the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # if there is already an inverse matrix, return it
        inv.m <- x$getInvM()
        if(!is.null(inv.m)) {
                message("getting cached data")
                return(inv.m)
        }
        
        # if the value of the inverse matrix is the NULL default value (as set
        # by SetM() in the makeCacheMatrix, or by creating the object), then the
        # inverse of the matrix should be calculated and cached in the special
        # matrix object.
        
        # store the matrix as data
        data <- x$getM()
        # calculate the inverse of the matrix stored in data
        inv.m <- solve(data, ...)
        # store the inverse matrix in the special matrix object
        x$setInvM(inv.m)
        # return the inverse matrix as calculated
        inv.m
        
}