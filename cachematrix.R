## This combination of functions calculates and stores the inverse of a matrix 
## in cache for reuse untill the matrix is changed.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## get dimensions of x and create an empty inverse matrix of that size
    dimx<- dim(x) 
    i <- matrix(rep(NA), nrow =dimx[1], ncol = dimx[2])
    setmatrix <- function(y){
        x<<- y
        i <- matrix(rep(NA), nrow =dimx[1], ncol = dimx[2])
    }
    getmatrix <- function () {
        x
    }
    setInv <- function (Inversem) {
        i <<-Inversem  
    }
    getInv <- function () {
        ## Compare the Inverse matrix to an NA matrix and return NULL if they are 
        ## identical
        li <-matrix(rep(NA), nrow =dimx[1], ncol = dimx[2])
        if(identical(i,li)) return(NULL)
        else return(i)
    }
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setInv = setInv,
         getInv = getInv)
        
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$getmatrix()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
