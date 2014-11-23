## The functions makeCacheMatrix and cacheSolve work together to make 
## it easier to cache large and time consuming computations and 
## datasets.

## makeCacheMatrix will calculate the mean of the vector and cache
## its value.

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get<-function() x
     setmatrix<-function(solve) m<<- solve
     getmatrix<-function() m
     list(set=set, get=get,
          setmatrix=setmatrix,
          getmatrix=getmatrix)
}

## cacheSolve will retrieve the mean by getting it from the cache.
## If the mean has not already been calculated then cacheSolve
## will calculate it and store the value of the mean in the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m<-x$getmatrix()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setmatrix(m)
     m
}
        
