# For a very large matrx, it may take too long to compute the inverse matrx, especially 
# if it has to be computed repeatedly (e.g. in a loop). If the contents of the matrix are not changing, 
# it may make senses to cache the value of the inverse so that when we need it again, it can be looked up 
# in the cache rather than recomputed. This Programming Assignment takes advantage of the scoping rules of 
# the R language and how they can be manipulated to preserve state inside of an R object

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
#       container cache for inverse of matrix  
        inv <- NULL
        
#       setter function to change matrix        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }


#       getter function to get matrix being stored 
        get <- function() x

#       setter function to cache matrix inverse
        setinv <- function(newinv) inv <<- newinv

#       getter function to get matrix inverse
        getinv <- function() inv

#       result to return as list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
#  only calculates inverse if not found
cacheSolve <- function(x, ...) {
        
                
#       using makeCacheMatrix getinv function to get reference to inv        
        m <- x$getinv()

        
#       if inverse is not null return inverse        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#       else get matrix, calculate, store and return inverse 
        data <- x$get()
        newinv <- solve(data, ...)
        x$setinv(newinv)
        newinv
}
