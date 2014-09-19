## Developed by Will Kashdan
## This assignment is for the Johns Hopkins R Programming Course

##makeCacheMatrix: matrix -> list
##Consumes: a matrix
##Produces: a list of functions 
##Purpose: To create a list of that represents a Matrix with an inverse Cache
## and create cache for the invese of a matrix
##Brief Description: This function takes in a matrix and creates 4 functions 
## to create a matrix with a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ##checks if a matrix is passed in
        if(!is.matrix(x)) stop("A Matrix Must be Given")
        solve(x)
        
        ##Set Function: Sets the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##returns the matrix
        get <- function() x
        ##sets the inverse matrix
        setsolve <- function(solve) m <<- solve
        ##returns the inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##cacheSolve: list -> matrix
##Consumes: a list that was created using the makeCacheMatrix function
##Produces: the inverse matrix
##Purpose: Return a matrix that is the inverse of 'x'
##Brief Description: This function calculates the inverse 
## of the matrix in the list in the prior function and the stores it
##in the list 

cacheSolve <- function(x, ...) {
        ##checks to see if the inverse was already solved and cached
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##gets the matrix
        data <- x$get()
        ##calculates the inverse
        m <- solve(data)
        ##sets the cache
        x$setsolve(m)
        ##returns the inverse
        m
}
