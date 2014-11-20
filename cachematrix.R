
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

compare <- function(x = matrix(), y = matrix()) {
       # get dimentsions of both parameters
       xs <- dim(x)
       ys <- dim(y)
       same <- xs != ys
       
       # if dimensions are the same then compare element values
       if (sum(same) == 0) {
         f <- x != y
         dim(f) <- prod(xs)
         sum(f) == 0
       }
       else {
         False
       }
    }

message("run test code")

# test code
v <- matrix(c(1,1,1,0.5), 2,2)
cm <- makeCacheMatrix(v)
cacheSolve(cm)
identity <- cm$get() %*% cacheSolve(cm)
# identity matrix
id  <- matrix(c(1,0,0,1),2,2)
## ident should be equal to identity
if (compare(identity,id)) { message("inverse was returned by cacheSolve") }




