## This R script is to create a square matrix according to the user's input
## and calculate and store (or cache) its inverse matrix so that when the
## cacheSolve function is called again, it won't re-calculate the inverse
## matrix but just return the cached value.
## If a new matrix is created, the old inverse matrix, which is cached, will 
## be removed and turn to NULL, waiting for the inverse matrix to be stored 
## when the inverse calculation is called.


## This function is to create a square matrix for inverse calculation and caching
## the result. For sub-functions are created:
## +  The set function is to change the old matrix to a new one. Inside, the
##    inv variable which assigned as the value of inversed matrix will be cleared
##    and turned to NULL, waiting for a new inverse matrix to be calculated
##    and stored. The matrix x will be assigned globally to the new matrix y.
## +  The get function is to return the current x matrix.
## +  The setinv is to be used to assign the inversed matrix calculated from 
##    cacheSOlve function to the variable inv.
## +  The getinv function is to return the value of inv

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y) {
      x<<-y
      inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function is to calculate the inverse matrix of x. If the x matrix is the
## same and inv has been calculated before, it will just return the cached value.

## The work flow of this function is first it assigns the value of inv with the value
## of x$getinv() (which is the inv value of makeCachMatrix, is NULL as stated in 
## the first line if cacheSolve hasn't run. If cacheSolve was run, the inv 
## in makeCacheMatrix would be assigned as the inv value calculated from cacheSolve)
## If inv is NULL, the function will calculate the inverse function using the 
## matrix from x$get(), and assigns its result to inv in x$setinv(inv).
## If inv is not NULL, it contains a value calculated from before. The function 
## will stop and return the cached inv value.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()  
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
    data<-x$get() 
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
