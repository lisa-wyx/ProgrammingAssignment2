## The pair of functions can cache the inverse of a matrix so
## it can be more quickly to calculate the inverse of matrix.

#Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y=matrix()){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(solve) m<<-solve
    getinverse<-function()m
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.If the inverse has already been 
## calculated,then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}