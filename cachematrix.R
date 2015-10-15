
# function to create and cache a matrix using a
# different environment

makeCacheMatrix <- function (x = matrix()) {
    
    # constructor
    trix <- NULL
    set <- function(y) {
        x <<- y
        trix <<- NULL
    }
    # methods
    get <- function() x
    set.trix<-function(solve) trix <<- solve
    get.trix<-function() trix
    list(set = set, get = get,
    set.trix = set.trix,
    get.trix = get.trix)
}

# solver function, returns inverse of the matrix

cacheSolve <- function(x=matrix(), ...) {
    
    # get matrix
    m <- x$get.trix()
    # if cached, return cache
    if (!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    # else get and solve for the inverse of x, then return inverse
    trix <- x$get()
    m <- solve(trix, ...)
    x$set.trix(m)
    m
}