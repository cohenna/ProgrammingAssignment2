## The follow functions will allow a user to cache the result of computing the 
## inverse (solve) of a matrix.  Caching these computations will result in 
## improved performance if the inverse results are frequently requested.
##
## To cache a result for a given matrix M, first call makeCacheMatrix(M) then 
## pass the result of makeCacheMatrix to cacheSolve:
##
##    M <- matrix(c(2, 4, 3, 1, 5, 7, 4, -4, 3), nrow=3, ncol=3)
##    Mc <- makeCacheMatrix(M)
##    inv <- cacheSolve(Mc)
## 
## The last line (namely, inv <- cacheSolve(Mc)) will return the inverse from
## the cache if the inverse has previously been calculated, thus, improving 
## performance by limiting the total number of times the inverse is really 
## calculated in the program lifecycle to 1. 
## 

## makeCacheMatrix creates a special "matrix", which is really a list containing functions which:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse
##   - get the value of the inverse
## 
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    # function for setting the value of the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # return the matrix
    get <- function() x
    
    # sets the inverse
    setinverse <- function(inverse) s <<- inverse
    
    # gets the inverse
    getinverse <- function() s
    
    # return the list of 4 functions, basically creating a special "cachable matrix" object
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve is a simple function which tries to retrieve the inverse of a makeCacheMatrix object.
## It first checks to see if the inverse has already been calculated.  If yes, then return the result
## immediately.  If not, then the inverse is calculated then cached for the next time this function
## is called.
cacheSolve <- function(x, ...) {
    # check whether the inverse has been calculated
    s <- x$getinverse()
    if(!is.null(s)) {
        # the inverse has been calculated, return it immediately
        message("getting cached data")
        return(s)
    }
    
    # the inverse has NOT been calculated.  Calculate, cache it, and return it now.
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
