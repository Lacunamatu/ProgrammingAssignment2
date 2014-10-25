## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    dimx<- dim(x)
    i <- matrix(rep(NA), nrow =dimx[1], ncol = dimx[2])
    setmatrix <- function(y){
        ## stuff
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
        li <-matrix(rep(NA), nrow =dimx[1], ncol = dimx[2])
        if(identical(i,li)) return(NULL)
        else return(i)
    }
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setInv = setInv,
         getInv = getInv)
        
}


## Write a short comment describing this function

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
