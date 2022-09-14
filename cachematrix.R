
## the code is composed of two major functions: "makeCacheMatrix" and "cacheSolve"
## "makeCacheMatrix" is a function which creates a special "matrix".
  ##It contains functions that are needed for "setting" and "getting" the vallues
    ## of the matrix and its inverse.
## "cacheSolve" is a function which calculates the inverse of the special “matrix” created by the "makeCacheMatrix" function 
    ## ;after first checking if the inverse has already been calculated.
       ## If that is the case, it ommits the computation and directly returns the inverse. 
         ## If not, then it calculates the inverse and sets it in the cache using the setinverse function.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        ## here we used the 'superassignment' operator (<<-),which assigns 
        ## a value to an object in the enclosing environment.
        ## Therefore, it starts with the enclosing frame, it
        ## works its way up towards the global environment until it finds a
        ## variable called x , and then assigns y to it. If it never locates 
        ## an object called x, then it creates one in the global environment.
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ##this part checks whether the inverse matrix has already been calculated
    ##if the answer is "yes",then it returns the message "getting cached data" 
    ## ,as well as the cached inverse matrix 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##if the answer is "no" then it
    ##"gets" that matrix via the "get" function, 
    ##it calculates the inverse of the matrix in question via the "solve" function,
    ##it sets the inverse matrix in the cache (x$setinverse(inv)),
    ##and also returns that inverse matrix
    new_matrix <- x$get()
    inv <- solve(new_matrix, ...)
    x$setinverse(inv)
    inv
}
