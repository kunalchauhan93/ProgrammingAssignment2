## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates and stores a matrix while 
## cacheSolve uses the matrix to calculate the inverse and display it

## Write a short comment describing this function
## makeCachematrix makes use of Lexical Scoping to define a function which
## creates a matrix and stores it in the memory each time the function is called

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL;
        set <- function(y){
                x <<- y;
                inv <<- NULL;
        }
        get <- function() x
        setmatrix <- function(solve) inv <<- solve
        getmatrix <- function() inv
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix);
}
## Write a short comment describing this function
## cacheSolve matrix calls the matrix defined in the makeCacheMatrix function
## and calculates the inverse of the matrix. However, if we call the function 
## with the same matrix input, we can see that the function fetches the result 
## from the memory cache i.e the result which has already been calculated before
## and displays it without solving for the inverse once again. Similarly, if we call the
## function with a new matrix, it will compute the inverse again and return the result.

cacheSolve <- function(x = matrix(), ...){
        inv <- x$getmatrix();
        if(!is.null(inv)){
                message("Retrieving cached result from memory...");
                return(inv);
        }
        newmat <- x$get();
        inv <- solve(newmat, ...);
        x$setmatrix(inv);
        inv;
}