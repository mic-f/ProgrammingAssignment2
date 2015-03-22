### The following function checks whether two matrices are the same
### It returns TRUE if they're the same and FALSE otherwise
matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)   
}


makeCacheMatrix <- function(x = matrix()) {
### This function is very similar to the function from given example
### The inverse_matrix stores computed inverse of matrix x
    inverse_matrix <- NULL
    set <- function(y){
        ### If new matrix happens to be the same as old one, we don't have to compute inverse
        if (!matequal(x,y)){
            x <<- y
            inverse_matrix <<- NULL    
        }
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## This function returns a matrix that is the inverse of "matrix" x (in fact the type of 
    ## argument x is list)
    inverse <- x$getinverse()
    ### If inverse was already computed, we take it from "matrix" x
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
