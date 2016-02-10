## The function makeCacheMatrix creates a special matrix which is a list containing a function to
## (1) set the value of the matrix, (2) get the value of the matrix, (3) set the inverse of the matrix,
## (4) get the inverse of the matrix. The second function calculates the matrix inverse.

## This is the first function

makeCacheMatrix <- function(x = matrix()) {     ##create function
        
        i <- NULL ##set object i to NULL for further use
        
        set <- function(y) {  ##create new function with argument y
                x <<- y ##set x to y in the parent enovironment (funciton for makeCakeMatrix)
                i <<- NULL ##set i to NULL in the parent enovironment (funciton for makeCakeMatrix)
        }
        get <- function() x ##new get function that return the value x (argument passed in makeCacheMatrix)
        setinverse <- function(inverse) i <<- inverse ##new setinverse function, setting i to the value "inverse" in the parent environment
        getinverse <- function() i ##new getinverse function returning value of i
        list(set = set, get = get, ##creating the list
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Function to calculate the inverse of the special matrix of the above function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        ## Return a matrix that is the inverse of 'x'
}
