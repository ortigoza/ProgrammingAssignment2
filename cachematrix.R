## This functions calculates the inverse of a matrix and store it
## to avoid further calculations if data not change.

#########EXAMPLE#############
# Create the schema
# m1 <- makeCacheMatrix()

# Fill with an invertible matrix
# m1$set(matrix(c(1,1,0,1,0,1,0,1,0),nrow=3,ncol=3))

# call cacheSolve function with m1 as parameter
# cacheSolve(m1)

# Fist time cacheSolve(m1) calculates the inverse
#      [,1] [,2] [,3]
#[1,]    1    0   -1
#[2,]    0    0    1
#[3,]   -1    1    1

# For the second and subsequent times the function gets cached data
#getting cached data
#       [,1] [,2] [,3]
#[1,]    1    0   -1
#[2,]    0    0    1
#[3,]   -1    1    1


## Creates an 'structure' to store, set and get data 
## i store the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(i) i <<- i  
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of x, 
cacheSolve <- function(x, ...) {

# Check and print if there's cached data.
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
# Otherwise function calculates the inverse, store and return the result
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}