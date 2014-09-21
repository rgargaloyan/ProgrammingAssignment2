#######################################################################
##  This function provides facilities to store the input matrix
##  as well as the invered matrix via closure. It's not necessarily
##  the smartest way to achieve the task, but evidently the recommended
##  for the scoping learning purposes
##
##  Input:   Matrix to be inversed
##  Output: Exposed functions (this is an object)
##
########################################################################
makeCachedMatrix <- function(x = matrix(),...) {
    inv <- NULL #the field to store the inversed matrix
    
    ###... Setter for the inbound matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ###... Getter for the inboud matrix
    get <- function() x
    
    ###... Setter for the inversed matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ###... Setter for the inversed matrix
    getinverse <- function() inv
    
    ###... Publish the list of exposed (public) functions
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


#######################################################################
##  This function provides way to inverse matrix with caching the inverse result
##  of the last matrix (being explicitly set)
##
##  Input:  Matrix store object (see makeCachedMatrix object definition)
##  Output: Inversed matrix
##
########################################################################
cachedInverse <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    ### This will return the inversed mtrix if available
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ### Get the user supplied matrix
    data <- x$get()
    
    ### Inverse the mtrix
    inverse <- solve(data, ...)
    
    ### Store inversed matrix in the "cache"
    x$setinverse(inverse)
    
    #returned the newly inversed matrix
    inverse
}

##   Sample call: Matrix needs to be square to be inversed
##
## f <- makeCachedMatrix(matrix(c(2, 4, 3, 1, 5, 7,9,11,15),nrow=3,ncol=3))
##
## cachedInverse(f) 
##