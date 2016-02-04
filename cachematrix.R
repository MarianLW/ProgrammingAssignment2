## Programming Assignment 2 for Week 3 of R programming.
## 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
 }

## This function first checks if the inverse has already been cached.
##If it has, it is retrieved, if it has not, the solve function is invoked.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

### basic test case
my_matrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) # create basic 2x2 matrix
cacheSolve(my_matrix) # calculate its inverse
my_matrix$get() %*% my_matrix$getsolve() # apply matrix multiplication to check result is identity matrix
cacheSolve(my_matrix) # call function again to confirm it is using cache

