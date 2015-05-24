## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that returns a list of functions

## Its puspose is to create a special "matrix" object that can cache its inverse.
## It Contains the following functions:

## setMatrix      set the value of a matrix

## getMatrix      get the value of a matrix

## setInverse     set the cache value (inverse of the matrix)
## getInverse     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
 	## m holds the cached value or NULL if nothing is cached
        ## initially nothing is cached so set it to NULL

        m <- NULL
        
        ## stores matrix
        setMatrix <- function(newValue) {
                x <<- newValue
               
		## since the matrix is assigned a new value clear the cache
                m <<- NULL
        }

        ## returns matrix
        getMatrix <- function() x

        ## set the cache value
        setInverse <- function(inv) m <<- inv

        ## get the cached value
        getInverse <- function() m
       
        ## return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated then cacheSolve would retrieve the inverse from the cache
## else it would calculate inverse and return it

cacheSolve <- function(x, ...) {
       
 	## get the cached value
        inv <- x$getInverse()
        
	## if cached value exists than return 
        if(!is.null(inv)) {
                message("Return cached data")

        } else {

        ## else get the matrix, calculate it's inverse and store it in the cache
        data <- x$getMatrix()
        inv <- solve(data)
        x$setInverse(inv)
        
	}
	
        ## return the inverse
        return(inv)	

}
