## ýt solves a given matrix to the function.

## it contains double operator to assign the variable to the enviranment and 
##consists of set, get, setInverse and getInverse function 

library(MASS) ## in order to calculate inverse a matrix regardless of its dim.
makeCacheMatrix<- function(x= matrix()){
        e<- NULL
        set <- function(y){
                x<<- y
                e<<- NULL
                
        }
        get<- function(){x}
        setInverse <- function(inverse) {e<<- inverse } 
        getInverse <- function() {
                inverse<- ginv(x)   ## to calculate inverse of a given matrix
                inverse%*%x
        }
        list(set = set, get = get, 
             setInverse= setInverse,
             getInverse= getInverse
        )
}

## This function utilize the cache data.

cachesolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        e<- x$getInverse()
        if(!is.null(e)) {
                message ("getting cached data")
                return(e)
        }
        data<- x$get()
        e<- solve(data, ...)
        x$setInverse(e)
        e
}
        

