
## Manage an object that memoizes the calculation of the inverse on a matrix


## Create a CacheMatrix object that has methods to store a matrix, 
## retrieve a matrix, retrieve the stored inverse or set the stored inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     # store new matrix in closure set inverse to NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }

     # return matrix stored in closure
     get <- function() x
     
     # store calculated inverse in closure
     setinverse <- function(inverse) inv <<- inverse
     
     # get stored inverse from closure
     getinverse <- function() inv
     
     list (set = set, 
           get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}



## function takes a CacheMatrix object and returns either the cached inverse
## or the calculated inverse after memoizing the calculation

cacheSolve <- function(x, ...) {
    # get cached inverse
    i <-  x$getinverse() 
    
    # if cached object is not null return it
    if (!is.null(i)) {
      message("getting cached inverse")
      return (i)
    }
    
    #else solve matrix for its inverse and then return it 
    # after cacheing result
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i   
}

