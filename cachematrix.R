
## This script contains two functions.
## These makes it so you don't have to calculate the same inverse multiple times

## makeCacheMatrix: creates the object that is able to store the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse to null (because it still needs to be calculated)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## Define the function to store the inverse once calculated
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        ## Return the list with the parameters to store the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: returns the inverse of a matrix
## if it was previously calculated, it just prints it

cacheSolve <- function(x, ...) {
        ## Get the variable that could have the previously calculated inverse
        m <- x$getinv()
        ## Check if it was calculated
        if(!is.null(m)){
                ## Return the previous calculation to save time
                message("getting cached inverse")
                return(m)
        }
        ## If not, then calculate the inverse and store it in the object
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        ## Return the calculated inverse
        m
}