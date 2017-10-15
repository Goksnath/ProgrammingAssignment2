#OVERALL WORKING OF CACHEMATRIX.R

## makeCacheMatrix accepts input in the form of matrix and returns output in the
    ## form of list which can be sent to CacheSolve Function
## by defaut, setinv and getinv is stored NULL and the input matrix is assigned
    ## to get()
## Once the computation is done in the CacheSolve function, the output is stored
    ## in this function with the help of setinv()
## CacheSolve function accepts arugument from the above function makeCacheMatrix
## Checks whether the matrix has been already inversed, if it is,
## Yes, then it returns the inverse of the matrix which has been stored in 
    ## makeCacheMatrix with the help of getinv()
## No, then it does the matrix inversion and returns the output as inversed 
    ## matrix as well as stores the output in makeCacheMatrix 
    ## with the help of setinv()


#FUNCTIONS USED IN THE SCRIPT:

##get()   - assigns the input args of makeCacheMatrix to x
##getinv()- output of CacheSolve can be retrieved here
##setinv()- output of CacheSolve stored in memory

#makeCacheMatrix()
## makeCacheMatrix accepts input in the form of matrix and returns output in the
    ## form of list which can be sent to CacheSolve Function
## by defaut, setinv and getinv is stored NULL and the input matrix is assigned
    ## to get()
## Once the computation is done in the CacheSolve function, the output is stored
    ## in this function with the help of setinv()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(matrix) m <<- matrix
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##CacheSolve()

## CacheSolve function accepts arugument from the above function makeCacheMatrix
## Checks whether the matrix has been already inversed, if it is,
## Yes, then it returns the inverse of the matrix which has been stored in 
    ## makeCacheMatrix with the help of getinv()
## No, then it does the matrix inversion and returns the output as inversed 
    ## matrix as well as stores the output in makeCacheMatrix 
    ## with the help of setinv()

cacheSolve <- function(x, ...) {
    
    ## Returns a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}