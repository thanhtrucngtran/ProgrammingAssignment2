## These 2 functions combined enable the user to inverse a matrix
## and store it in the cache, so that in case of multiple
## uses it takes less space on the RAM.

## The function below, makeCacheMatrix, contains a list of 4 functions:
## 1. set() - sets the value of the matrix, changing the original value if any
## 2. get() - gets the value of the matrix and displays it
## 3. setinverse() - sets the value of the inverse matrix
## 4. getinverse() - gets the value of the inverse matrix
## This function can store up to 2 matrices, one stored with set() and the 
## second one stored with setinverse(). Note that setinverse() does not 
## automatically calculate the inverse of the matrix, but just stores a second 
## matrix. To get the inverse, we need to use the second function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y){ 
        x <<- y         
        inv <<- NULL    
    }
    get <- function()x  
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) 
}


## The function below, cacheSolve, inverse a matrix in case this is not already
## stored in the cache.
## If there is a matrix already stored with x$setinverse(inv), the function 
## returns the value without the need of recalculating it.
## In case no matrices have been stored with x$setinverse(inv), the function
## calculates the inverse of the matrix, stores it with x$setinverse(inv) and
## returns it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse() 
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}


