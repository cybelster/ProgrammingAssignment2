## Put comments here that give an overall description of what your
## functions do

## # makeCacheMatrix creates a list containing a function to 
# a. set the value of the matrix 
# b. get the value of the matrix 
# c. set the value of inverse of the matrix 
# d. get the value of inverse of the matrix 


makeCacheMatrix <- function(a = matrix()) { 
     inv <- NULL 
     set <- function(b) { 
        a <<- b 
         inv <<- NULL 
     } 
     get <- function() a
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}




## # the following function inverses the Matrix if not already performed or retrieves from cache


cacheSolve <- function(a, ...) { 
     inv <- a$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     data <- a$get() 
     inv <- solve(data) 
     a$setinverse(inv)
     inv 
}
