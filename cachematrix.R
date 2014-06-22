## Caching the Inverse of a Matrix


makeCacheMatrix <- function(x = matrix()) 
{
    ## This function creates a special "matrix" object that can cache its inverse
    
    m<-NULL
    set<-function(y)
    {
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

cacheSolve <- function(x=matrix(), ...) 
{
    ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ## If the inverse has already been calculated (and the matrix has not changed), 
    ## then the cachesolve should retrieve the inverse from the cache.
    
    m<-x$getmatrix()
    
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}