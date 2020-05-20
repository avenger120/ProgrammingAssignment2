## Coding project for R class where functions are made to display the inverse of a matrix and cache it.
## If that matrix inverse has already been calcualted, the cached data will be displayed instead of
## doing the full calculation again.

## This function creates a list contatining a function that will: set the value of the matix,
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix has already been calculated.
## If it has it returns the text "getting cached data" and prints the cached data.
## Otherwise it will calculate the inverse and display that as well as caching it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
