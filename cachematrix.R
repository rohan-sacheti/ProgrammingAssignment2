## Put comments here that give an overall description of what your
## Examples
## > a1 <- c(3, 2, 5)
## > a2 <- c(2, 3, 2)
## > a3 <- c(5, 2, 4)
## > A <- rbind(a1, a2, a3)
## > A
##    [,1] [,2] [,3]
## a1    3    2    5
## a2    2    3    2
## a3    5    2    4
##
## > m <- makeCacheMatrix(A)
## > cacheSolve(m)
##               a1          a2         a3
## [1,] -0.29629630 -0.07407407  0.4074074
## [2,] -0.07407407  0.48148148 -0.1481481
## [3,]  0.40740741 -0.14814815 -0.1851852
## > cacheSolve(m)
## [1] "Getting cached data"
##               a1          a2         a3
## [1,] -0.29629630 -0.07407407  0.4074074
## [2,] -0.07407407  0.48148148 -0.1481481
## [3,]  0.40740741 -0.14814815 -0.1851852

## This funciton creates a matrix object with 4 sub funtions
## Set method for the Matrix
## Get method for the matrix
## Set method for the inverse
## Get method for the inverse

# @param x is a matrix
# @return matrix object

# example input
## > A
##    [,1] [,2] [,3]
## a1    3    2    5
## a2    2    3    2
## a3    5    2    4
## > a = makeCacheMatrix(A)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL;

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    getInverse <- function() inv

    setInverse <- function(y) {
        inv <<- y
    }

    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## @param x is the matrix object
## returns the inverse of the matrix
## This function returns the inverse of a matrix object
## It caches the calculated value and returns it when called multiple times

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInverse()

        if (!is.null(inv)) {
            print('Getting cached data')
            inv
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
