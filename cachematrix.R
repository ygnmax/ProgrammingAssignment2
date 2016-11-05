## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        m<<-NULL#assign null to m 
        set <- function(y){
                x<<-y#after set(y), x=y
                m<<-NULL#after set(y), m=null
        }
        get<-function() x#assign x to get
        setsolve <- function(solve) m<<-solve# get inverse matrix
        getsolve <- function() m #assign m to getsolve
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## calculates the inverse of the matrix created with the above function.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()#assign getsolve to m
        if(!is.null(m)) {   
                message("getting cached data")#if m is not NULL, it will show the message
                return(m)
        } #the cache
        data <- x$get() #assign matrix to data
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
