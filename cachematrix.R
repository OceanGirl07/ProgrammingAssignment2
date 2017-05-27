### Programming Assignment 2: Lexical Scoping
## 5-26-17


## below are two functions that cache the inverse of a square, invertible matrix.

## this function creates a special matrix object that can cache its inverse.  More specifically,
## it returns a set of functions (set, get, set_inverse, get_inverse) within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize
        m <- NULL
        
        ## set matrix
        set <- function(y){
                x <<- y ## assigns value to x in environment different than current environment (parent environment)
                m <<- NULL ## assigns value to m from parent environment
                ## clears any values that have been cached by a prior execution.
        }
        ## get matrix
        get <- function(){
                # return matrix
                x
        }
        
        ## set inverse of matrix
        set_inverse <- function(inverse){
                m <<- inverse  ## assigns value to m from parent environment
        }
        
        ## get inverse of matrix
        get_inverse <- function(){
                ## return inverse
                m
        }
        
        ## Return list of methods, each element in the list is named, 
        ##which allows us to use the $ form of the extraction operator below to access functions by name.
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## this function computes the inverse of the special matrix returned by makeCacheMatrix.
## if the inverse has already been calculated and the matrix has not changed, then it will return 
## the inverse that is cached.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$get_inverse() 
        
        ## check to see if the inverse is already calculated (if m is not null, we have a valid, cached inverse)
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse is not cached, get the matrix
        data <- x$get() #$fetches data
        
        ## calculate inverse matrix using R command "solve"
        m <- solve(data)
        
        ## set inverse
        x$set_inverse(m)
        
        ## return matrix
        m
}



