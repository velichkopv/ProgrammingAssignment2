
## This function creates matrix 'X' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function (y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function (reverse) r<<-reverse
        getreverse <- function() r
        list(set=set,get=get,setreverse=setreverse,getreverse=getreverse)
}

## This function computes the inverse of
## matrix 'X' returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        reverse <- x$getreverse()
        if(!is.null(reverse)) {
                message ("getting cashed data")
                return(reverse)
        }
        data <- x$get()
        require(MASS)
        if (!is.matrix(data)) 
                data <- as.matrix(data)
        datasvd <- svd(data, ...)
        r <- datasvd$v %*% (1/datasvd$d * t(datasvd$u))
        x$setreverse(r)
        r
}
## Return a matrix that is the inverse of 'x'

