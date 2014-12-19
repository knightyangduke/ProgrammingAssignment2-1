## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##construct a special matrix containing elements (x), compoed into a matrix of (row)*(column)
##This special matrix can be used to cache the inverse matrix of x(row*column)
makeCacheMatrix <- function(x = numeric(),row= 1,column=1) {
        library('MASS')
        m <- NULL
        set <- function(y) {
                x <<- matrix(y,row,column,byrow=TRUE)
                m <<- NULL
        }
        getmatrix <- function() {
        matrix(x,row,column,byrow=TRUE)
        }
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

##cacheSolve get the inverse of Special Matrix x, defined by makeCacheMatrix function
##if there is already a calculated inverse, then return it,otherwise calculate the inverse matrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- ginv(data, ...)
        x$setinverse(m)
        m
}
