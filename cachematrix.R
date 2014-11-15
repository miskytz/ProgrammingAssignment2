## This is a program include  a pair of functions that cache the inverse of a matrix 
## functions do


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  
        m <- NULL                        ## ininitialize 
        set_matrix <- function(y) {x <<- y       ## set the matrix 
                m <<- NULL
         }
        get_matrix <- function() x              ## get the matrix         
        setsolve <- function(solve) m <<- solve       ## set the inverse of the matrix 
        getsolve <- function() m                     ## get the inverse of the matrix
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             setsolve = setsolve,
             getsolve = getsolve)
  
  
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## if the matrix inverse already have been got,returm the inverse.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get_matrix()                 ##get the inverse
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
