## creating a invertible sqaure matrix
B = matrix( c(20,30,60,40,100,120,99,67,31), nrow=3,ncol=3)

## Function to create the special matrix

makeCacheMatrix <- function(x = matrix()) {
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) minverse <<- solve
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Creating the special matrix for B
## Binverse by getting the Return Object for makeCacheMatrix

Binverse <- makeCacheMatrix(B)

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        data <- x$get()
        minverse <- solve(data, ...)
        x$setinverse(minverse)
        minverse
}

## pass Binverse to cacheSolve
cacheSolve(Binverse)
## output

##      [,1]   [,2]         [,3]
##[1,]  0.02321429 -0.050  0.033928571
##[2,] -0.01452068  0.025 -0.007659774
##[3,]  0.01127820  0.000 -0.003759398 

## check 

identical(solve(B),cacheSolve(Binverse))
## Output
## getting cached data
## [1] TRUE
