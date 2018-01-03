## Coursera R Programming Week 3 Assignment 2
## GitHub user: 7856

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inv inverse matrix as null
        inv <- NULL
        ## define set function for assigning new value to matrix x in parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## define get function to get the value of matrix x
        get <- function() x
        
        ## define setinverse function to assign value of inv in the parent environment
        setinverse <- function(inverse) inv <<- inverse
        
        ## define getinverse function to get the value of matrix inv
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## if the inverse has already been calculated, it gets the inverse from the cache and skips the computation
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if not, it calculates the inverse and sets the value in the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        return(inv)
}



