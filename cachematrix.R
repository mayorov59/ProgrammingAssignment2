##Matrix inversion is usually a costly computation and there may be some benefit to
##caching the inverse of a matrix rather than compute it repeatedly.


## makeCacheMatrix creates cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    msolve <- function(invm) {
        inv <<- solve(x)
    }
    getinv <- function() {
        return(inv)
    }
    list(
        set = set,get = get,getinv = getinv,msolve = msolve
    )
}


##cacheSolve either computes inverse matrix and cache it or if this was done earlier returns cached object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (is.null(inv)) { x$msolve() }
    else {
        message ("cached matrix")
        return (inv)
    }
}
